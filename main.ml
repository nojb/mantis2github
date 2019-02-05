let ostr = function
  | Some x -> `String x
  | None -> `Null

let hashtbl_of_list l =
  let h = Hashtbl.create (List.length l) in
  List.iter (fun (id, x) -> Hashtbl.add h id x) l;
  h

let user_map =
  let h = Hashtbl.create 17 in
  let ic = open_in "user_map.txt" in
  let rec loop () =
    match String.trim (input_line ic) with
    | s when s = "" || s.[0] = '#' ->
        loop ()
    | s ->
        let s1, s2 =
          try
            Scanf.sscanf s "%S %s%!" (fun s1 s2 -> s1, s2)
          with _ ->
            failwith (Printf.sprintf "Error while parsing users map: %S" s)
        in
        Hashtbl.add h s1 s2;
        loop ()
    | exception End_of_file ->
        h
  in
  loop ()

type gh =
  {
    owner: string;
    repo: string;
    token: string option;
  }

module Curl = struct
  let root = "https://api.github.com"

  let with_params url = function
    | [] -> url
    | l ->
        url ^ "?" ^ String.concat "&" (List.map (fun (k, v) -> String.concat "=" [k; v]) l)

  module J = Yojson.Basic.Util

  let curl ?(x = "GET") ?(h = []) ?d url =
    let headers =
      match h with
      | [] -> ""
      | l ->
          String.concat " "
            (List.map (fun (k, v) -> "-H \"" ^ k ^ ": " ^ v ^ "\"") l) ^ " "
    in
    let data =
      match d with
      | None -> ""
      | Some d ->
          let filename, oc = Filename.open_temp_file "curl" "data" in
          set_binary_mode_out oc true;
          output_string oc d;
          close_out oc;
          "--data-binary @" ^ filename ^ " "
    in
    let cmd = Printf.sprintf "curl -Ss %s%s-X %s %s/%s" data headers x root url in
    Printf.eprintf "+ %s\n%!" cmd;
    let tmp = Filename.temp_file "curl" "out" in
    assert (Sys.command (Printf.sprintf "%s > %s" cmd tmp) = 0);
    let ic = open_in_bin tmp in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let json = Yojson.Basic.from_string s in
    Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json;
    match J.member "message" json with
    | `String s -> failwith s
    | _ ->
        json

  let list_milestones {owner; repo; token} =
    let url = with_params (Printf.sprintf "repos/%s/%s/milestones" owner repo) ["state", "all"] in
    let f json =
      let title = json |> J.member "title" |> J.to_string in
      let number = json |> J.member "number" |> J.to_int in
      Some (title, number)
    in
    let h = match token with Some token -> ["Authorization", "token " ^ token] | None -> [] in
    curl ~h url |> J.to_list |> J.filter_map f

  let is_imported {owner; repo; token} id =
    let h = ["Accept", "application/vnd.github.golden-comet-preview+json"] in
    let url = Printf.sprintf "repos/%s/%s/import/issues/%d" owner repo id in
    let h = match token with Some token -> ("Authorization", "token " ^ token) :: h | None -> h in
    match curl ~h url |> J.member "status" |> J.to_string with
    | "imported" -> true
    | "failed" -> failwith "Import failed!"
    | _ -> false

  let start_import {owner; repo; token} json =
    let h = ["Accept", "application/vnd.github.golden-comet-preview+json"] in
    let h = match token with Some token -> ("Authorization", "token " ^ token) :: h | None -> h in
    let url = Printf.sprintf "repos/%s/%s/import/issues" owner repo in
    let d = Yojson.Basic.pretty_to_string json in
    Printf.eprintf "%s\n%!" d;
    curl ~x:"POST" ~h ~d url |> J.member "id" |> J.to_int
end

module Status = struct
  type t =
    | New
    | Feedback
    | Acknowledged
    | Confirmed
    | Assigned
    | Resolved
    | Closed

  let of_int = function
    | 10 -> New
    | 20 -> Feedback
    | 30 -> Acknowledged
    | 40 -> Confirmed
    | 50 -> Assigned
    | 80 -> Resolved
    | 90 -> Closed
    | n -> failwith (Printf.sprintf "Unexpected status code: %d" n)

  let to_string = function
    | New -> "new"
    | Feedback -> "feedback"
    | Acknowledged -> "acknowledged"
    | Confirmed -> "confirmed"
    | Assigned -> "assigned"
    | Resolved -> "resolved"
    | Closed -> "closed"

  let is_closed = function
    | Resolved | Closed -> true
    | _ -> false

  let to_json st =
    let module J = Yojson.Basic in
    `String (to_string st)
end

module Note = struct
  type t =
    {
      reporter: string option;
      text: string;
      last_modified: string;
      date_submitted: string;
    }

  let to_json {reporter = _; text; last_modified = _; date_submitted} =
    let module J = Yojson.Basic in
    `Assoc
      [
        "body", `String text;
        "created_at", `String date_submitted;
      ]
end

module Issue = struct
  type t =
    {
      summary: string;
      priority: string;
      category: string;
      date_submitted: string;
      last_updated: string;
      reporter: string option;
      handler: string option;
      description: string;
      steps_to_reproduce: string;
      additional_information: string;
      target_version: string;
      notes: Note.t list;
      status: Status.t;
      closed_at: string option;
    }

  let body ~description ~steps_to_reproduce:_ ~additional_information:_ =
    description

  let labels ~priority:_ ~category:_ ~status:_ =
    []

  let milestone ~target_version:_ =
    `Null

  let to_json
      {
        summary;
        priority;
        category;
        date_submitted;
        last_updated;
        reporter = _;
        handler;
        description;
        steps_to_reproduce;
        additional_information;
        target_version;
        notes;
        status;
        closed_at;
      }
    =
    let module J = Yojson.Basic in
    let body = body ~description ~steps_to_reproduce ~additional_information in
    let labels = labels ~priority ~category ~status in
    let milestone = milestone ~target_version in
    let closed = Status.is_closed status in
    let issue =
      [
        "title", `String summary;
        "body", `String body;
        "created_at", `String date_submitted;
        "closed_at", ostr closed_at;
        "updated_at", `String last_updated;
        "assignee", ostr handler;
        "milestone", milestone;
        "closed", `Bool closed;
        "labels", `List labels;
      ]
    in
    let comments = List.map Note.to_json notes in
    `Assoc ["issue", `Assoc issue; "comments", `List comments]
end

open Mysql

let exec dbd ~f query =
  Mysql.map (Mysql.exec dbd query) ~f

let timestamp s =
  let {Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _} =
    Unix.gmtime (float_of_string s)
  in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let main dbd =
  let categories =
    let f = function
      | [|Some id; Some name|] ->
          int_of_string id, name
      | _ ->
          failwith "Unexpected response when querying categories"
    in
    let r = exec dbd ~f "SELECT id, name FROM mantis_category_table;" in
    Hashtbl.of_seq (List.to_seq r)
  in
  let users =
    let f = function
      | [|Some id; Some user_name|] ->
          let gh_user =
            match Hashtbl.find_opt user_map user_name with
            | Some s -> s
            | None ->
                (* Printf.eprintf "WARNING: user_map: missing user: %S\n%!" *)
                (*   user_name; *)
                user_name ^ "@Mantis"
          in
          int_of_string id, gh_user
      | _ ->
          failwith "Unexpected response when querying users"
    in
    let r =
      exec dbd ~f "SELECT id, username FROM mantis_user_table;"
    in
    hashtbl_of_list r
  in
  let texts =
    let f = function
      | [|Some id; Some description; Some steps_to_reproduce; Some additional_information|] ->
          int_of_string id, (description, steps_to_reproduce, additional_information)
      | _ ->
          failwith "Unexpected response when querying bug texts"
    in
    let r =
      exec dbd ~f "SELECT id, description, steps_to_reproduce, additional_information \
                   FROM mantis_bug_text_table;"
    in
    hashtbl_of_list r
  in
  let notes =
    let texts =
      let f = function
        | [|Some id; Some note|] ->
            int_of_string id, note
        | _ ->
            failwith "Unexpected response when querying notes texts"
      in
      let r = exec dbd ~f "SELECT id, note FROM mantis_bugnote_text_table;" in
      hashtbl_of_list r
    in
    let f = function
      | [|Some bug_id; Some reporter_id; Some bugnote_text_id;
          Some last_modified; Some date_submitted|] ->
          let reporter = Hashtbl.find_opt users (int_of_string reporter_id) in
          let text = Hashtbl.find texts (int_of_string bugnote_text_id) in
          let last_modified = timestamp last_modified in
          let date_submitted = timestamp date_submitted in
          int_of_string bug_id, {Note.reporter; text; last_modified; date_submitted}
      | _ ->
          failwith "Unexpected response when querying notes"
    in
    let r =
      exec dbd ~f "SELECT bug_id, reporter_id, bugnote_text_id, \
                   last_modified, date_submitted \
                   FROM mantis_bugnote_table ORDER BY date_submitted DESC;"
    in
    hashtbl_of_list r
  in
  let statuses =
    let f = function
      | [|Some bug_id; Some date_modified; Some new_value|] ->
          int_of_string bug_id,
          (timestamp date_modified, Status.of_int (int_of_string new_value))
      | _ ->
          failwith "Unexpected response when querying history"
    in
    let r =
      exec dbd ~f
        "SELECT bug_id, date_modified, new_value FROM mantis_bug_history_table \
         WHERE field_name = 'status' ORDER BY date_modified ASC;"
    in
    let h = Hashtbl.create (List.length r) in
    List.iter (fun (id, x) -> Hashtbl.replace h id x) r;
    h
  in
  let query =
    "SELECT id, summary, priority, category_id, date_submitted, last_updated, \
     reporter_id, handler_id, bug_text_id, target_version, status \
     FROM mantis_bug_table ORDER BY id;"
  in
  let f = function
    | [|Some id; Some summary; Some priority; Some category_id;
        Some date_submitted; Some last_updated; Some reporter_id;
        Some handler_id; Some bug_text_id; Some target_version;
        Some status|] ->
        let id = int_of_string id in
        let category = Hashtbl.find categories (int_of_string category_id) in
        let reporter = Hashtbl.find_opt users (int_of_string reporter_id) in
        let handler = Hashtbl.find_opt users (int_of_string handler_id) in
        let description, steps_to_reproduce, additional_information =
          Hashtbl.find texts (int_of_string bug_text_id)
        in
        let notes = Hashtbl.find_all notes id in
        let status = Status.of_int (int_of_string status) in
        let closed_at =
          match Hashtbl.find_opt statuses id with
          | None -> None
          | Some (closed_at, st) ->
              assert (st = status);
              if Status.is_closed status then Some closed_at else None
        in
        id, { Issue.summary; priority; category;
              date_submitted = timestamp date_submitted;
              last_updated = timestamp last_updated; reporter; handler;
              description; steps_to_reproduce; additional_information; target_version;
              notes; status; closed_at }
    | _ ->
        failwith "Unexpected response when querying bugs"
  in
  exec dbd ~f query

let connect db =
  try
    let dbd = Mysql.connect db in
    Mysql.set_charset dbd "utf8";
    dbd
  with Mysql.Error s ->
    prerr_endline s;
    exit 2

let fetch db =
  let dbd = connect db in
  match main dbd with
  | issues ->
      Mysql.disconnect dbd;
      issues
  | exception e ->
      Printf.eprintf "ERROR: %s\n%!" (Printexc.to_string e);
      Mysql.disconnect dbd;
      exit 2

let extract db =
  let f (_, issue) =
    let json = Issue.to_json issue in
    Printf.printf "%a\n" (Yojson.pretty_to_channel ~std:true) json
  in
  List.iter f (fetch db)

let milestones gh =
  let l = Curl.list_milestones gh in
  List.iter print_endline (List.map fst l)

let create_issue gh issue =
  let id = Curl.start_import gh (Issue.to_json issue) in
  let rec loop () =
    Unix.sleep 1;
    if Curl.is_imported gh id then prerr_endline "Done!"
    else (prerr_endline "Waiting..."; loop ())
  in
  loop ()

let create_issues gh db bug_ids =
  let issues = hashtbl_of_list (fetch db) in
  List.iter (fun id -> create_issue gh (Hashtbl.find issues id)) bug_ids

open Cmdliner

let db dbhost dbname dbport dbpwd dbuser =
  {dbhost; dbname; dbport; dbpwd; dbuser; dbsocket = None}

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
  Term.(const db $ dbhost $ dbname $ dbport $ dbpwd $ dbuser)

let extract_cmd =
  let doc = "Extract Mantis into JSON" in
  let exits = Term.default_exits in
  Term.(const extract $ db_t),
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
  let github owner repo token = {owner; repo; token} in
  Term.(const github $ owner $ repo $ token)

let milestones_cmd =
  let doc = "List milestones in ocaml/ocaml" in
  Term.(const milestones $ github_t),
  Term.info "milestones" ~doc

let bug_ids_t =
  let doc = "Mantis bug numbers." in
  Arg.(value & pos_all int [] & info [] ~doc)

let create_issue_cmd =
  let doc = "Create an issue" in
  Term.(const create_issues $ github_t $ db_t $ bug_ids_t),
  Term.info "create-issue" ~doc ~sdocs:Manpage.s_common_options

let default_cmd =
  let doc = "a Mantis => GH migration tool" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Term.info "mantis2github" ~version:"v0.1" ~doc ~sdocs ~exits

let cmds = [extract_cmd; milestones_cmd; create_issue_cmd]

let () =
  Term.(exit (eval_choice default_cmd cmds))
