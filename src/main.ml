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

let milestones (token, owner, repo) =
  match Github.Milestone.list ?token ~owner ~repo () with
  | Some l ->
      List.iter print_endline (List.map fst l)
  | None ->
      ()

module List = struct
  include List

  let rec truncate n = function
    | [] -> []
    | _ :: l when n > 0 -> truncate (pred n) l
    | _ as l -> l
end

let import verbose (token, owner, repo) db assignee from nmax ids =
  let gh_user =
    match assignee with
    | None -> begin fun s -> try Some (gh_user s) with Not_found -> None end
    | Some _ as x -> begin fun _ -> x end
  in
  let f db =
    let issues = Mantis.fetch db in
    let ids =
      if ids = [] then Hashtbl.fold (fun id _ acc -> id :: acc) issues [] else ids
    in
    let ids = List.filter (fun id -> id >= from) ids in
    let ids = List.truncate nmax ids in
    let ids = List.sort Stdlib.compare ids in
    let f (total_retries, total_time, count) id =
      let issue = Hashtbl.find issues id in
      let starttime = Unix.gettimeofday () in
      let res =
        Github.Issue.import ~verbose ?token ~owner ~repo
          (Migrate.Issue.migrate ~gh_user issue)
      in
      let endtime = Unix.gettimeofday () in
      let dt = endtime -. starttime in
      let total_time = total_time +. dt in
      let retries = match res with Ok (_, retries) | Error retries -> retries in
      let total_retries = total_retries + retries in
      let gh_id =
        match res with
        | Ok (id, _) -> string_of_int id
        | Error _ -> "ERR"
      in
      Printf.printf "%4d %4s %2d %2d %6d %6.1f %6.1f %6.1f\n%!"
        id gh_id retries (truncate (float total_retries /. float count))
        total_retries dt (total_time /. float count) total_time;
      (total_retries, total_time, succ count)
    in
    List.fold_left f (0, 0., 0) ids
  in
  Mantis.Db.use db (fun db -> ignore (f db))

open Cmdliner

let db_t =
  let docs = Manpage.s_options in
  let dbhost =
    let doc = "Database hostname." in
    Arg.(value & opt (some string) (Some "127.0.0.1") & info ["dbhost"] ~docs ~doc)
  in
  let dbname =
    let doc = "Database name." in
    Arg.(value & opt (some string) (Some "db") & info ["dbname"] ~docs ~doc)
  in
  let dbport =
    let doc = "Database port." in
    Arg.(value & opt (some int) None & info ["dbport"] ~docs ~doc)
  in
  let dbpwd =
    let doc = "Database password." in
    Arg.(value & opt (some string) None & info ["dbpassword"] ~docs ~doc)
  in
  let dbuser =
    let doc = "Database username." in
    Arg.(value & opt (some string) (Some "root") & info ["dbusername"] ~docs ~doc)
  in
  let db dbhost dbname dbport dbpwd dbuser =
    {Mysql.dbhost; dbname; dbport; dbpwd; dbuser; dbsocket = None}
  in
  Term.(const db $ dbhost $ dbname $ dbport $ dbpwd $ dbuser)

let verbose_t =
  let doc = "Be verbose." in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)

let bug_ids_t =
  let doc = "Mantis bug numbers." in
  Arg.(value & pos_all int [] & info [] ~doc)

let extract_cmd =
  let doc = "Extract Mantis into JSON" in
  let exits = Term.default_exits in
  Term.(const extract $ db_t $ bug_ids_t),
  Term.info "extract" ~doc ~sdocs:Manpage.s_common_options ~exits

let github_t =
  let docs = Manpage.s_options in
  let owner =
    let doc = "Github owner." in
    Arg.(required & opt (some string) None & info ["owner"] ~docs ~doc)
  in
  let repo =
    let doc = "Github repo." in
    Arg.(required & opt (some string) None & info ["repo"] ~docs ~doc)
  in
  let token =
    let doc = "Github token." in
    Arg.(value & opt (some string) None & info ["token"] ~docs ~doc)
  in
  let github owner repo token = (token, owner, repo) in
  Term.(const github $ owner $ repo $ token)

let milestones_cmd =
  let doc = "List Github milestones." in
  Term.(const milestones $ github_t),
  Term.info "milestones" ~doc

let assignee_t =
  let doc = "Override assignee." in
  Arg.(value & opt (some string) None & info ["assignee"] ~doc)

let nmax_t =
  let doc = "Max number of issues to migrate." in
  Arg.(value & opt int max_int & info ["max"] ~doc)

let from_t =
  let doc = "Bug number to start importing from." in
  Arg.(value & opt int 0 & info ["from"] ~doc)

let import_cmd =
  let doc = "Import issues." in
  Term.(const import $ verbose_t $ github_t $ db_t $ assignee_t $ from_t $ nmax_t $ bug_ids_t),
  Term.info "import" ~doc

let default_cmd =
  let doc = "a Mantis => Github migration tool" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "mantis2github" ~version:"v0.1" ~doc ~sdocs ~exits

let cmds = [extract_cmd; milestones_cmd; import_cmd]

let () =
  Term.(exit (eval_choice default_cmd cmds))
