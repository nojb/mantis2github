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

  let rec drop n = function
    | [] -> []
    | _ :: l when n > 0 -> drop (pred n) l
    | _ as l -> l

  let rec truncate n = function
    | [] -> []
    | x :: l when n > 0 -> x :: truncate (pred n) l
    | _ -> []
end

let with_out s f =
  let oc = open_out_bin s in
  match f oc with
  | r ->
      close_out oc; r
  | exception exn ->
      close_out oc; raise exn

let with_in s f =
  let ic = open_in_bin s in
  match f ic with
  | r ->
      close_in ic; r
  | exception exn ->
      close_in ic; raise exn

let issues =
  let db =
    {
      Mysql.dbuser = Some "root";
      dbpwd = None;
      dbhost = Some "127.0.0.1";
      dbport = None;
      dbsocket = None;
      dbname = Some "db";
    }
  in
  Mantis.Db.use db Mantis.fetch

let extract () =
  let f _ issue =
    let json = Mantis.Issue.to_json issue in
    Printf.printf "%a\n" (Yojson.Basic.pretty_to_channel ~std:true) json
  in
  Hashtbl.iter f issues

let compute_assignment next l =
  let module S = Set.Make (struct type t = int let compare = Stdlib.compare end) in
  let rec go assigned unassigned next = function
    | id :: ids as ids' ->
        if id < next then
          go assigned (S.add id unassigned) next ids
        else if id = next then
          go ((id, next) :: assigned) unassigned (succ next) ids
        else (* id > next *)
          begin match S.min_elt_opt unassigned with
          | None ->
              go ((id, next) :: assigned) unassigned (succ next) ids
          | Some id ->
              go ((id, next) :: assigned) (S.remove id unassigned) (succ next) ids'
          end
    | [] ->
        let rec loop unassigned assigned next =
          match S.min_elt_opt unassigned with
          | None -> assigned
          | Some id -> loop (S.remove id unassigned) ((id, next) :: assigned) (succ next)
        in
        loop unassigned assigned next
  in
  List.sort Stdlib.compare l
  |> go [] S.empty next
  |> List.sort (fun (_, x) (_, y) -> Stdlib.compare x y)

let () =
  let t n l l' = assert (compute_assignment n l = l') in
  t 2 [1; 2; 3; 4] [(2, 2); (3, 3); (4, 4); (1, 5)];
  t 2 [4; 5] [(4, 2); (5, 3)];
  t 3 [3; 5] [(3, 3); (5, 4)];
  t 9 [3; 5] [(3, 9); (5, 10)];
  t 1 [3; 5] [(3, 1); (5, 2)];
  t 3 [5; 3] [(3, 3); (5, 4)]

let output_assignment oc a =
  List.iter (fun (id, gh_id) -> Printf.fprintf oc "%4d %4d\n" id gh_id) a

type safepoint =
  | Start_import
  | Gist_created of (string * string) list
  | Waiting_for_import of (string * string) list * Github.Issue.waiting

type gh =
  {
    token: string;
    owner: string;
    repo: string;
  }

type state =
  {
    finished: int;
    pt: safepoint;
  }

let save_state gh st next =
  with_out "resume_info.dat" (fun oc -> Marshal.to_channel oc (gh, st, next) [])

let read_state () : gh * state * int =
  with_in "resume_info.dat" (fun ic -> Marshal.from_channel ic)

let get_next verbose {token; owner; repo} =
  match Github.Issue.count ~verbose ~token ~owner ~repo () with
  | None ->
      failwith "Could not count issues, cannot proceed!"
  | Some n ->
      succ n

let import verbose ({token; owner; repo} as gh) txt state next =
  let n = Hashtbl.length issues in
  let a =
    Hashtbl.fold (fun id _ acc -> id :: acc) issues []
    |> compute_assignment next
  in
  begin match txt with
  | None -> ()
  | Some txt ->
      with_out txt (fun oc -> output_assignment oc a)
  end;
  let gh_ids =
    let h = Hashtbl.create (List.length a) in
    List.iter (fun (id, gh_id) -> Hashtbl.add h id gh_id) a;
    Hashtbl.find h
  in
  let rec f a ({finished; pt} as state) =
    match a, pt with
    | [], Start_import -> Ok ()
    | (id, _) :: _, Start_import ->
        let issue = Hashtbl.find issues id in
        let _, gh_gist = Migrate.Issue.migrate ~owner ~repo ~gh_ids issue in
        begin match gh_gist with
        | None ->
            f a {finished; pt = Gist_created []}
        | Some gist ->
            begin match Github.Gist.create ~verbose ~token gist with
            | None ->
                Error state
            | Some gist_urls ->
                f a {finished; pt = Gist_created gist_urls}
            end
        end
    | [], (Gist_created _ | Waiting_for_import _) ->
        assert false
    | (id, _) :: _, Gist_created gist_urls ->
        let issue = Hashtbl.find issues id in
        let gh_issue, _ = Migrate.Issue.migrate ~owner ~repo ~gh_ids issue in
        begin match Github.Issue.import ~verbose ~token ~owner ~repo (gh_issue gist_urls) with
        | None ->
            Error state
        | Some w ->
            f a {finished; pt = Waiting_for_import (gist_urls, w)}
        end
    | (id, gh_id) :: pending, Waiting_for_import (gist_urls, w) ->
        begin match Github.Issue.check_imported ~verbose ~token ~owner ~repo w with
        | Waiting w ->
            f a {finished; pt = Waiting_for_import (gist_urls, w)}
        | Failed {retry = true} ->
            Error state
        | Failed {retry = false} ->
            Error {finished; pt = Gist_created gist_urls}
        | Success gh_id' ->
            if gh_id <> gh_id' then
              Printf.ksprintf failwith
                "Github ID mismatch! (id=%d,gh_id=%d,gh_id'=%d)" id gh_id gh_id';
            let finished = succ finished in
            Printf.printf "%4d => %4d (%d%%)\n%!" id gh_id
              (truncate (float finished /. float n));
            f pending {finished; pt = Start_import}
        end
  in
  match f (List.drop state.finished a) state with
  | Error state ->
      save_state gh state next
  | Ok () ->
      ()

let import_one verbose {token; owner; repo} id =
  let gh_ids _ = raise Not_found in
  let issue = Hashtbl.find issues id in
  let _, gh_gist = Migrate.Issue.migrate ~owner ~repo ~gh_ids issue in
  let gist_urls =
    match gh_gist with
    | None ->
        []
    | Some gist ->
        begin match Github.Gist.create ~verbose ~token gist with
        | None ->
            failwith "could not create gist"
        | Some gist_urls ->
            gist_urls
        end
  in
  let issue = Hashtbl.find issues id in
  let gh_issue, _ = Migrate.Issue.migrate ~owner ~repo ~gh_ids issue in
  let w =
    match Github.Issue.import ~verbose ~token ~owner ~repo (gh_issue gist_urls) with
    | None ->
        failwith "could not start import"
    | Some w ->
        w
  in
  let rec loop w =
    match Github.Issue.check_imported ~verbose ~token ~owner ~repo w with
    | Waiting w ->
        loop w
    | Failed _ ->
        failwith "Failed"
    | Success gh_id ->
        Printf.printf "%4d => %4d\n%!" id gh_id
  in
  loop w

let resume verbose =
  let gh, state, next = read_state () in
  import verbose gh None state next

let import verbose gh txt =
  import verbose gh txt
    {finished = 0; pt = Start_import} (get_next verbose gh)

open Cmdliner

let verbose_t =
  let doc = "Be verbose." in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)

let extract_cmd =
  let doc = "Extract Mantis into JSON" in
  Term.(const extract $ const ()), Term.info "extract" ~doc

let github_t =
  let docs = Manpage.s_options in
  let repo =
    let doc = "Github repository." in
    Arg.(required & pos 0 (some (pair ~sep:'/' string string)) None &
         info [] ~docs ~doc ~docv:"OWNER/REPO")
  in
  let token =
    let doc = "Github token." in
    Arg.(required & opt (some string) None & info ["token"] ~docs ~doc ~docv:"TOKEN")
  in
  let github token (owner, repo) = {token; owner; repo} in
  Term.(const github $ token $ repo)

let o_t =
  let doc = "Output assignment." in
  Arg.(value & opt (some string) None & info ["o"] ~doc)

let import_cmd =
  let doc = "Import issues." in
  Term.(const import $ verbose_t $ github_t $ o_t),
  Term.info "import" ~doc

let id_t =
  let doc = "Bug number." in
  Arg.(required & pos 1 (some int) None & info [] ~doc)

let import_one_cmd =
  let doc = "Import single issues." in
  Term.(const import_one $ verbose_t $ github_t $ id_t),
  Term.info "import-one" ~doc

let resume_cmd =
  let doc = "Resume issues." in
  Term.(const resume $ verbose_t),
  Term.info "resume" ~doc

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
    import_one_cmd;
    resume_cmd;
  ]

let () =
  Term.(exit (eval_choice default_cmd cmds))
