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

let extract db = function
  | [] ->
      let f (_, issue) =
        let json = Mantis.Issue.to_json issue in
        Printf.printf "%a\n" (Yojson.pretty_to_channel ~std:true) json
      in
      List.iter f (Mantis.fetch db)
  | bug_ids ->
      let issues = Mantis.Hashtbl.of_list (Mantis.fetch db) in
      List.iter (fun id ->
          match Hashtbl.find_opt issues id with
          | None ->
              Printf.eprintf "No Mantis issue found with id %d\n%!" id
          | Some issue ->
              let json = Mantis.Issue.to_json issue in
              Printf.printf "%a\n%!" (Yojson.pretty_to_channel ~std:true) json
        ) bug_ids

let milestones (token, owner, repo) =
  match Github.Milestone.list ?token ~owner ~repo () with
  | Some l ->
      List.iter print_endline (List.map fst l)
  | None ->
      ()

let create_issues (token, owner, repo) db bug_ids =
  let issues = Mantis.Hashtbl.of_list (Mantis.fetch db) in
  List.iter (fun id ->
      let _ =
        Github.Issue.import ?token ~owner ~repo
          (Migrate.Issue.migrate ~gh_user:(fun _ -> None)
             (Hashtbl.find issues id))
      in
      ()
    ) bug_ids

let migrate verbose (token, owner, repo) db assignee from nmax =
  let issues = Mantis.Hashtbl.of_list (Mantis.fetch db) in
  let n = Hashtbl.length issues in
  let rec loop total_retries total count idx =
    if count >= min n nmax then ()
    else begin
      match Hashtbl.find_opt issues idx with
      | None ->
          loop total_retries total count (succ idx)
      | Some issue ->
          let starttime = Unix.gettimeofday () in
          let res =
            let gh_user =
              match assignee with
              | None -> begin fun s -> try Some (gh_user s) with Not_found -> None end
              | Some _ as x -> begin fun _ -> x end
            in
            Github.Issue.import ~verbose ?token ~owner ~repo
              (Migrate.Issue.migrate ~gh_user issue)
          in
          let endtime = Unix.gettimeofday () in
          let delta = endtime -. starttime in
          let total = total +. delta in
          let retries = match res with Ok (_, retries) | Error retries -> retries in
          let total_retries = total_retries + retries in
          let id =
            match res with
            | Ok (id, _) -> string_of_int id
            | Error _ -> "ERR"
          in
          let count = succ count in
          Printf.printf "%4d %4s %2d %2d %6d %6.1f %6.1f %6.1f\n%!"
            issue.Mantis.Issue.id id retries (truncate (float total_retries /. float count))
            total_retries delta (total /. float count) total;
          loop total_retries total count (succ idx)
    end
  in
  loop 0 0. 0 from

open Cmdliner

let db_t =
  let docs = Manpage.s_options in
  let dbhost =
    let doc = "Server hostname." in
    Arg.(value & opt (some string) (Some "127.0.0.1") & info ["host"] ~docs ~doc)
  in
  let dbname =
    let doc = "Database name." in
    Arg.(value & opt (some string) (Some "db") & info ["name"] ~docs ~doc)
  in
  let dbport =
    let doc = "Server port." in
    Arg.(value & opt (some int) None & info ["port"] ~docs ~doc)
  in
  let dbpwd =
    let doc = "Server password." in
    Arg.(value & opt (some string) None & info ["password"] ~docs ~doc)
  in
  let dbuser =
    let doc = "Server username." in
    Arg.(value & opt (some string) (Some "root") & info ["username"] ~docs ~doc)
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
  let doc = "List milestones in ocaml/ocaml" in
  Term.(const milestones $ github_t),
  Term.info "milestones" ~doc

let create_issue_cmd =
  let doc = "Create an issue." in
  Term.(const create_issues $ github_t $ db_t $ bug_ids_t),
  Term.info "create-issue" ~doc ~sdocs:Manpage.s_common_options

let force_assignee_t =
  let doc = "Override assignee." in
  Arg.(value & opt (some string) None & info ["assignee"] ~doc)

let nmax_t =
  let doc = "Max number of issues to migrate." in
  Arg.(value & opt int max_int & info ["max"] ~doc)

let from_t =
  let doc = "Bug number to start importing from." in
  Arg.(value & opt int 0 & info ["from"] ~doc)

let migrate_cmd =
  let doc = "Migrate all issues." in
  Term.(const migrate $ verbose_t $ github_t $ db_t $ force_assignee_t $ from_t $ nmax_t),
  Term.info "migrate" ~doc

let default_cmd =
  let doc = "a Mantis => GH migration tool" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Term.info "mantis2github" ~version:"v0.1" ~doc ~sdocs ~exits

let cmds = [extract_cmd; milestones_cmd; create_issue_cmd; migrate_cmd]

let () =
  Term.(exit (eval_choice default_cmd cmds))
