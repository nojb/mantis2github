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

module Hashtbl = struct
  include Hashtbl

  let keys h =
    fold (fun k _ acc -> k :: acc) h []

  let of_assoc l =
    let h = Hashtbl.create (List.length l) in
    List.iter (fun (k, v) -> add h k v) l;
    h
end

let interactive = ref true

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
      dbname = Some "mantis";
    }
  in
  Mantis.Db.use db Mantis.fetch

let extract ids =
  let gh_ids id = id in
  let f id =
    let issue = Hashtbl.find issues id in
    let issue, gist = Migrate.Issue.migrate ("ocaml", "ocaml") ~gh_ids ~milestones:[] issue in
    let urls =
      match gist with
      | None -> []
      | Some {Github.Gist.files; _} -> files
    in
    let json = Github.Issue.to_json (issue urls) in
    Printf.printf "%a\n" (Yojson.Basic.pretty_to_channel ~std:true) json
  in
  List.iter f ids

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

let append_to_log (id, gh_id) =
  let s = Printf.sprintf "%-4d %-4d" id gh_id in
  let fd = Unix.openfile "_log" [O_WRONLY; O_APPEND; O_CREAT] 0o644 in
  let oc = Unix.out_channel_of_descr fd in
  output_string oc s;
  output_char oc '\n';
  close_out oc;
  prerr_endline s

let really_read_log ~name =
  let log = Hashtbl.create 13 in
  let ic = open_in name in
  let rec loop () =
    match input_line ic with
    | s -> Scanf.sscanf s "%d %d" (Hashtbl.add log); loop ()
    | exception End_of_file -> ()
  in
  loop ();
  log

let read_log ?(name = "_log") () =
  if not (Sys.file_exists name) then
    fun _ -> None
  else
    let log = really_read_log ~name in
    Hashtbl.find_opt log

let existing_number = 2324

let import verbose token repo =
  let a = Hashtbl.keys issues |> compute_assignment (succ existing_number) in
  let gh_ids = Hashtbl.find (Hashtbl.of_assoc a) in
  let milestones = Github.Milestones.list ~verbose ?token repo in
  let a =
    let log = read_log () in
    List.filter (fun (id, gh_id) ->
        match log id with
        | None -> true
        | Some gh_id' ->
          if gh_id' <> gh_id then
            Printf.ksprintf failwith
              "Inconsistent _log (id=%d,gh_id=%d,gh_id'=%d), aborting" id gh_id gh_id'
          else
            false
      ) a
  in
  let do_import id gh_id gh_issue =
    let iid = Github.Issue.import ~verbose ?token repo gh_issue in
    let rec loop sleep =
      Unix.sleep sleep;
      match Github.Issue.check_imported ~verbose ?token repo iid with
      | Pending ->
        loop (2 * sleep)
      | Failed ->
        Printf.ksprintf failwith "Import of #%d failed!" id
      | Imported gh_id' ->
        append_to_log (id, gh_id');
        if gh_id <> gh_id' then
          Printf.ksprintf failwith
            "Github ID mismatch! (id=%d,gh_id=%d,gh_id'=%d)" id gh_id gh_id'
    in
    loop 1
  in
  let a =
    match a with
    | [] -> []
    | (id, gh_id) :: a as a0 ->
      if Github.Issue.exists ~verbose ?token repo gh_id then begin
        Printf.eprintf "PR#%d [#%d] already exists, skipping\n%!" id gh_id;
        append_to_log (id, gh_id);
        a
      end else begin
        match Github.Gist.last ~verbose ?token () with
        | None -> a0
        | Some (description, gist_id) ->
            let gh_id' =
              description
              |> String.split_on_char '/'
              |> List.rev
              |> List.hd
              |> int_of_string
            in
            if gh_id = gh_id' then begin
              Printf.eprintf "Stale gist %s for PR#%d [#%d] found, removing\n%!"
                gist_id id gh_id;
              Github.Gist.delete ~verbose ?token gist_id
            end;
            a0
      end
  in
  let f (id, gh_id) =
    let issue = Hashtbl.find issues id in
    let gh_issue, gist = Migrate.Issue.migrate repo ~gh_ids ~milestones issue in
    let raw_urls =
      match gist with
      | None -> []
      | Some gist -> Github.Gist.create ~verbose ?token gist
    in
    do_import id gh_id (gh_issue raw_urls);
    if !interactive then begin
      Printf.printf "Issue imported. Continue? [Y/N/C] %!";
      match String.lowercase_ascii (read_line ()) with
      | "n" | "no" -> raise Exit
      | "c" | "cont" -> interactive := false
      | _ -> ()
    end;
    if !Github.total_num_requests >= 4900 then raise Exit;
  in
  try List.iter f a with Exit -> ()

let color =
  let x =
    ref [ 0xfd597a;
          0xa1dcf3;
          0xc1f2e7;
          0x92d3fc;
          0x1c528d;
          0x1c8a8d;
          0x6895fa;
          0xcca266;
          0xdeb887;
          0xa39e98;
          0x7a7875;
          0xd4d1c8;
          0x948572;
          0x9a4340;
          0x4de4c0;
          0x10100e;
          0xffff96;
          0xfbde57;
          0xffc0cb;
          0xe6e6fa;
          0xffaa00;
          0xffff96;
          0xff32c8;
          0x00ff7d;
          0xeda17c;
          0x00ced1;
          0x468499;
          0xfff68f;
          0x66cdaa;
          0xf6546a;
          0xffc3a0;
          0x20b2aa;
          0xffd700;
          0xffa500;
          0xfa8072;
          0xff7373;
          0x008080;
          0xfa8072;
          0xffffff;
          0xef606b;
          0x319d9d;
          0xe5c100 ]
  in
  fun () ->
    match !x with
    | c :: r -> x := r; c
    | [] -> failwith "No more colors!"

let check verbose token repo force =
  if false then begin
    let tmp = Filename.temp_file "curl" "out" in
    List.iter (fun (_, gh) ->
        assert (0 = Printf.ksprintf Sys.command "curl -Ss -w '%%{http_code}\n' -o /dev/null https://api.github.com/users/%s > %s" gh tmp);
        let code =
          let ic = open_in tmp in
          let n = int_of_string (input_line ic) in
          close_in ic;
          n
        in
        if code <> 200 then Printf.printf "Missing GH user: %s\n%!" gh
      ) Migrate.mantis2gh_users
  end;
  let gh_issues =
    let gh_ids id = id in
    Hashtbl.fold (fun _ issue acc ->
        let gh_issue, gh_gist = Migrate.Issue.migrate repo ~gh_ids ~milestones:[] issue in
        let raw_urls =
          match gh_gist with
          | None -> []
          | Some {Github.Gist.files; _} -> files
        in
        gh_issue raw_urls :: acc
      ) issues []
  in
  let all_labels =
    List.fold_left (fun acc {Github.Issue.issue = {Github.Issue.Issue.labels; _}; _} ->
        List.rev_append labels acc
      ) [] gh_issues
    |> List.sort_uniq Stdlib.compare
  in
  let gh_labels = Github.Labels.list ~verbose ?token repo in
  let first = ref true in
  List.iter (fun lab ->
      if not (List.mem lab gh_labels) then begin
        if !first then (first := false; print_endline "Missing labels:");
        print_endline lab;
        if force then Github.Labels.create ~verbose ?token repo lab (color ())
      end
    ) all_labels;
  let gh_assignees = Github.Assignees.list ~verbose ?token repo in
  let all_assignees =
    List.fold_left (fun acc {Github.Issue.issue = {Github.Issue.Issue.assignee; _}; _} ->
        match assignee with
        | Some s -> s :: acc
        | None -> acc
      ) [] gh_issues
    |> List.sort_uniq Stdlib.compare
  in
  first := true;
  List.iter (fun s ->
      if not (List.mem s gh_assignees) then begin
        if !first then (first := false; print_endline "Missing assignees:");
        print_endline s
      end
    ) all_assignees;
  let gh_milestones = Github.Milestones.list ~verbose ?token repo in
  let all_milestones =
    Hashtbl.fold (fun _ {Mantis.Issue.target_version; _} acc ->
        match Re.exec_opt Migrate.milestone_re (String.trim target_version) with
        | None ->
            acc
        | Some g ->
            Re.Group.get g 0 :: acc
      ) issues []
    |> List.sort_uniq Stdlib.compare
  in
  first := true;
  List.iter (fun s ->
      if not (List.mem_assoc s gh_milestones) then begin
        if !first then (first := false; print_endline "Missing milestones:");
        print_endline s;
        if force then Github.Milestones.create ~verbose ?token repo s |> ignore
      end
    ) all_milestones

let relabel verbose token repo =
  let log = really_read_log ~name:"issue_mapping.txt" in
  Hashtbl.iter (fun id gh_id ->
      let issue = Hashtbl.find issues id in
      let labels =
        match issue.Mantis.Issue.severity, issue.Mantis.Issue.status with
        | Mantis.Severity.Feature, _ ->
            ["feature-wish"]
        | _, Mantis.Status.New ->
            []
        | _ ->
            ["bug"]
      in
      if labels <> [] then Github.Issue.add_labels ~verbose ?token repo gh_id labels
    ) log

open Cmdliner

let verbose_t =
  let doc = "Be verbose." in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)

let extract_cmd =
  let doc = "Extract Mantis into JSON" in
  Term.(const extract $ Arg.(value & pos_all int [] & info [] ~docv:"ID")),
  Term.info "extract" ~doc

let token_t =
  let doc = "Github token." in
  Arg.(value & opt (some string) None & info ["token"] ~doc ~docv:"TOKEN")

let repo_t =
  let doc = "Github repository." in
  Arg.(required & pos 0 (some (pair ~sep:'/' string string)) None & info [] ~doc ~docv:"OWNER/REPO")

let import_cmd =
  let doc = "Import issues." in
  Term.(const import $ verbose_t $ token_t $ repo_t),
  Term.info "import" ~doc

let force_t =
  let doc = "Create" in
  Arg.(value & flag & info ["force"] ~doc)

let check_cmd =
  let doc = "Check." in
  Term.(const check $ verbose_t $ token_t $ repo_t $ force_t),
  Term.info "check" ~doc

let default_cmd =
  let doc = "a Mantis => Github migration tool" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "mantis2github" ~version:"v0.1" ~doc

let relabel_cmd =
  let doc = "Relabel." in
  Term.(const relabel $ verbose_t $ token_t $ repo_t),
  Term.info "relabel" ~doc

let cmds =
  [
    extract_cmd;
    import_cmd;
    check_cmd;
    relabel_cmd;
  ]

let () =
  Term.(exit (eval_choice default_cmd cmds))
