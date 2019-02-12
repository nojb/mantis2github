(* This is free and unencumbered software released into the public domain.

   Anyone is free to copy, modify, publish, use, compile, sell, or
   distribute this software, either in source code form or as a compiled
   binary, for any purpose, commercial or non-commercial, and by any
   means.

   In jurisdictions that recognize copyright laws, the author or authors
   of this software dedicate any and all copyright interest in the
   software to the public domain. We make this dedication for the benefit
   of the public at large and to the detriment of our heirs and
   successors. We intend this dedication to be an overt act of
   relinquishment in perpetuity of all present and future rights to this
   software under copyright law.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
   OTHER DEALINGS IN THE SOFTWARE.

   For more information, please refer to <http://unlicense.org> *)

(* This list contains a mapping between Mantis user names and GH usernames.

   Names appearing on this list are susceptible of being assigned migrated
   issues. They must have sufficient permissions to do so and be a subset of
   caml-devel subscribers. *)

module List = struct
  include List

  let rec truncate n = function
    | [] -> []
    | x :: l when n > 0 -> x :: truncate (pred n) l
    | _ -> []
end

let with_out s f =
  let oc = open_out s in
  match f oc with
  | r ->
      close_out oc;
      Printf.eprintf "[OK] Wrote %s.\n%!" s;
      r
  | exception exn ->
      close_out oc;
      Printf.eprintf "[ERR] Could not write %s.\n%!" s;
      raise exn

let gh_user = function
  | "administrator" -> "bactrian"
  | "xleroy" -> "xavierleroy"
  (* | "remy" -> "diremy" *)
  | "doligez" -> "damiendoligez"
  | "garrigue" -> "garrigue"
  | "frisch" -> "alainfrisch"
  | "weis" -> "pierreweis"
  (* | "mauny" -> "mauny" *)
  | "avsm" -> "avsm"
  | "dra" -> "dra27"
  (* | "fpottier" -> "fpottier" *)
  | "maranget" -> "maranget"
  | "Sebastien_Hinderer"  | "shindere" -> "shindere"
  | "yallop" -> "yallop"
  | "chambart" -> "chambart"
  | "shinwell" -> "mshinwell"
  | "lefessan" -> "lefessan"
  (* | "protz" -> "protz" *)
  | "lpw25" -> "lpw25"
  | "gasche" -> "gasche"
  (* | "hongboz" -> "bobzhang" *)
  (* | "jacques-henri.jourdan" -> "jhjourdan" *)
  | "def" -> "let-def"
  | "stedolan" -> "stedolan"
  | "trefis" -> "trefis"
  | "damien" -> "damiendoligez"
  | "nojb" | "nojebar" -> "nojb"
  | "octachron" -> "Octachron"
  | "Armael" -> "Armael"
  | "dim" -> "diml"
  (* | "guesdon" -> "zoggy" *)
  | _ -> raise Not_found

let extract db ids =
  let f db =
    let issues = Mantis.fetch db in
    if ids = [] then
      let f _ issue =
        let json = Mantis.Issue.to_json issue in
        Printf.printf "%a\n" (Yojson.Basic.pretty_to_channel ~std:true) json
      in
      Hashtbl.iter f issues
    else
      List.iter (fun id ->
          match Hashtbl.find_opt issues id with
          | None ->
              Printf.eprintf "No Mantis issue found with id %d\n%!" id
          | Some issue ->
              let json = Mantis.Issue.to_json issue in
              Printf.printf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json
        ) ids
  in
  Mantis.Db.use db f

module IntSet = Set.Make (struct type t = int let compare = Stdlib.compare end)

let assignment issues next =
  let rec go assigned unassigned next = function
    | id :: ids as ids' ->
      if id < next then
        go assigned (IntSet.add id unassigned) next ids
      else if id = next then
        go ((id, next) :: assigned) unassigned (succ next) ids
      else (* id > next *)
        begin match IntSet.min_elt_opt unassigned with
        | None ->
            Printf.eprintf
              "[WARNING] gap cannot be filled by unassigned issues (id=%d,next=%d)\n%!"
              id next;
            go ((id, next) :: assigned) unassigned (succ next) ids
        | Some id ->
            go ((id, next) :: assigned) (IntSet.remove id unassigned) (succ next) ids'
        end
    | [] ->
      let rec loop unassigned assigned next =
        match IntSet.min_elt_opt unassigned with
        | None -> assigned
        | Some id -> loop (IntSet.remove id unassigned) ((id, next) :: assigned) (succ next)
      in
      loop unassigned assigned next
  in
  Hashtbl.fold (fun id _ acc -> id :: acc) issues []
  |> List.sort Stdlib.compare
  |> go [] IntSet.empty next
  |> List.sort (fun (_, gh_id1) (_, gh_id2) -> Stdlib.compare gh_id1 gh_id2)

let import verbose (token, owner, repo) db assignee bug_ids =
  let next =
    match Github.Issue.count ~verbose ?token ~owner ~repo () with
    | None ->
        failwith "Could not count issues!"
    | Some n ->
        succ n
  in
  let issues =
    let issues = Mantis.Db.use db Mantis.fetch in
    if bug_ids = [] then issues
    else begin
      let h = Hashtbl.create (List.length bug_ids) in
      List.iter (fun id ->
          match Hashtbl.find_opt issues id with
          | None -> Printf.eprintf "WARNING: no bug with id=%d\n%!" id
          | Some issue -> Hashtbl.replace h id issue
        ) bug_ids;
      h
    end
  in
  let a = assignment issues next in
  with_out "assign.txt" (fun oc ->
      List.iter (fun (id, gh_id) ->
          Printf.fprintf oc "%4d %4d\n" id gh_id
        ) a
    );
  let gh_user =
    match assignee with
    | None -> begin fun s -> try Some (gh_user s) with Not_found -> None end
    | Some _ as x -> begin fun _ -> x end
  in
  let gh_ids =
    let h = Hashtbl.create (List.length a) in
    List.iter (fun (id, gh_id) -> Hashtbl.add h id gh_id) a;
    Hashtbl.find h
  in
  let f (total_time, count) (id, gh_id) =
    let issue = Hashtbl.find issues id in
    let gh_issue, gh_gist = Migrate.Issue.migrate ~owner ~repo ~gh_user ~gh_ids issue in
    let starttime = Unix.gettimeofday () in
    let gist_urls =
      match gh_gist with
      | None -> "", []
      | Some gist ->
          begin match Github.Gist.create ~verbose ?token gist with
          | None -> failwith "Gist upload failed! Abort."
          | Some urls -> urls
          end
    in
    match Github.Issue.import ~verbose ?token ~owner ~repo (gh_issue gist_urls) with
    | None ->
        failwith "Import failed! Abort"
    | Some gh_id' ->
        if gh_id <> gh_id' then
          Printf.ksprintf failwith "Github ID mismatch! (id=%d,gh_id=%d,gh_id'=%d)"
            issue.Mantis.Issue.id gh_id gh_id';
        let endtime = Unix.gettimeofday () in
        let dt = endtime -. starttime in
        let total_time = total_time +. dt in
        Printf.printf "%4d %4d %6.1f %6.1f %dh%02d\n%!"
          issue.Mantis.Issue.id gh_id dt (total_time /. float count)
          (truncate (total_time /. 3600.)) (truncate (total_time /. 60.));
        (total_time, succ count)
  in
  List.fold_left f (0., 0) a |> ignore

open Cmdliner

let db =
  {
    Mysql.dbuser = Some "root";
    dbpwd = None;
    dbhost = Some "127.0.0.1";
    dbport = None;
    dbsocket = None;
    dbname = Some "db";
  }

let verbose_t =
  let doc = "Be verbose." in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)

let bug_ids_t =
  let doc = "Mantis bug numbers." in
  Arg.(value & pos_right 0 int [] & info [] ~doc)

let extract_cmd =
  let doc = "Extract Mantis into JSON" in
  let exits = Term.default_exits in
  Term.(const extract $ const db $ bug_ids_t),
  Term.info "extract" ~doc ~sdocs:Manpage.s_common_options ~exits

let github_t =
  let docs = Manpage.s_options in
  let repo =
    let doc = "Github repository." in
    Arg.(required & pos 0 (some (pair ~sep:'/' string string)) None &
         info [] ~docs ~doc ~docv:"OWNER/REPO")
  in
  let token =
    let doc = "Github token." in
    Arg.(value & opt (some string) None & info ["token"] ~docs ~doc ~docv:"TOKEN")
  in
  let github token (owner, repo) = (token, owner, repo) in
  Term.(const github $ token $ repo)

let assignee_t =
  let doc = "Override assignee." in
  Arg.(value & opt (some string) None & info ["assignee"] ~doc)

let import_cmd =
  let doc = "Import issues." in
  Term.(const import $ verbose_t $ github_t $ const db $ assignee_t $ bug_ids_t),
  Term.info "import" ~doc

let default_cmd =
  let doc = "a Mantis => Github migration tool" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "mantis2github" ~version:"v0.1" ~doc ~sdocs ~exits

let cmds =
  [
    extract_cmd;
    import_cmd;
  ]

let () =
  Term.(exit (eval_choice default_cmd cmds))
