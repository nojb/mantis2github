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

module J = Yojson.Basic.Util

module Hashtbl = struct
  include Hashtbl
  let of_list l =
    let h = Hashtbl.create (List.length l) in
    List.iter (fun (id, x) -> Hashtbl.add h id x) l;
    h
end

let ostr s o l =
  match o with
  | Some x -> (s, `String x) :: l
  | None -> l

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

  let curl meth ?(headers = []) ?data ?(params = []) {owner; repo; token} route =
    let headers =
      match token with
      | Some token -> ("Authorization", "token " ^ token) :: headers
      | None -> headers
    in
    let headers = ("Accept", "application/vnd.github.golden-comet-preview+json") :: headers in
    let headers =
      match headers with
      | [] -> ""
      | l ->
          String.concat " "
            (List.map (fun (k, v) -> "-H \"" ^ k ^ ": " ^ v ^ "\"") l) ^ " "
    in
    let data =
      match data with
      | None -> ""
      | Some data ->
          let filename, oc = Filename.open_temp_file "curl" "data" in
          set_binary_mode_out oc true;
          output_string oc data;
          close_out oc;
          "--data-binary @" ^ filename ^ " "
    in
    let params =
      match params with
      | [] -> ""
      | l -> "?" ^ String.concat "&" (List.map (fun (k, v) -> String.concat "=" [k; v]) l)
    in
    let cmd =
      Printf.sprintf "curl -Ss %s%s-X %s %s/repos/%s/%s/%s%s"
        data headers meth root owner repo route params
    in
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

  let get ?headers ?params gh route = curl "GET" ?headers ?params gh route
  let post ?headers ?data gh route = curl "POST" ?headers ?data gh route

  let list_milestones gh =
    let f json =
      let title = json |> J.member "title" |> J.to_string in
      let number = json |> J.member "number" |> J.to_int in
      Some (title, number)
    in
    get ~params:["state", "all"] gh "milestones" |> J.to_list |> J.filter_map f

  let is_imported gh id =
    let route = Printf.sprintf "import/issues/%d" id in
    match get gh route |> J.member "status" |> J.to_string with
    | "imported" -> true
    | "failed" -> failwith "Import failed!"
    | _ -> false

  let start_import gh json =
    let data = Yojson.Basic.pretty_to_string json in
    Printf.eprintf "%s\n%!" data;
    post ~data gh "import/issues" |> J.member "id" |> J.to_int

  let create_issue gh json =
    let id = start_import gh json in
    let rec loop n =
      if is_imported gh id then prerr_endline "Done!"
      else begin
        Printf.eprintf "Waiting %ds...\n" n;
        Unix.sleep n;
        loop (2 * n)
      end
    in
    loop 1
end

module Labels = struct
  type t =
    | Duplicate
    | No_change_required
    | Unable_to_reproduce
    | Wontfix

  let to_string = function
    | Duplicate -> "duplicate"
    | No_change_required -> "no change required"
    | Unable_to_reproduce -> "unable to reproduce"
    | Wontfix -> "wontfix"
end

module Resolution = struct
  type t =
    | Open
    | Fixed
    | Reopened
    | Unable_to_duplicate
    | Not_fixable
    | Duplicate
    | Not_a_bug
    | Suspended
    | Wont_fix

  let to_int = function
    | Open -> 10
    | Fixed -> 20
    | Reopened -> 30
    | Unable_to_duplicate -> 40
    | Not_fixable -> 50
    | Duplicate -> 60
    | Not_a_bug -> 70
    | Suspended -> 80
    | Wont_fix -> 90

  let of_int = function
    | 10 -> Open
    | 20 -> Fixed
    | 30 -> Reopened
    | 40 -> Unable_to_duplicate
    | 50 -> Not_fixable
    | 60 -> Duplicate
    | 70 -> Not_a_bug
    | 80 -> Suspended
    | 90 -> Wont_fix
    | n -> Printf.ksprintf failwith "Unexpected resolution code: %d" n
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
    | n -> Printf.ksprintf failwith "Unexpected status code: %d" n

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
    `Assoc
      [
        "body", `String text;
        "created_at", `String date_submitted;
      ]
end

module Issue = struct
  type t =
    {
      id: int;
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
      version: string;
      target_version: string;
      fixed_in_version: string;
      notes: Note.t list;
      status: Status.t;
      closed_at: string option;
      resolution: Resolution.t;
      related: int list;
      tags: string list;
    }

  let fence = String.make 6 '`'

  let body ~id ?(reporter = "") ~description ~steps_to_reproduce ~additional_information =
    let buf = Buffer.create 101 in
    let add title s =
      let s = String.trim s in
      if s <> "" then
        Printf.bprintf buf "**%s**\n%s\n%s\n%s\n" title fence s fence
    in
    add "ID" (Printf.sprintf "%07d" id);
    add "Reporter" reporter;
    add "Description" description;
    add "Steps to reproduce" steps_to_reproduce;
    add "Additional information" additional_information;
    Buffer.contents buf

  let labels ~priority:_ ~category:_ ~status:_ =
    []

  let milestone ~target_version:_ =
    `Null

  let to_json
      {
        id;
        summary;
        priority;
        category;
        date_submitted;
        last_updated;
        reporter;
        handler;
        description;
        steps_to_reproduce;
        additional_information;
        version = _;
        target_version;
        fixed_in_version = _;
        notes;
        status;
        closed_at;
        resolution = _;
        related = _;
        tags = _;
      }
    =
    let body = body ~id ?reporter ~description ~steps_to_reproduce ~additional_information in
    let labels = labels ~priority ~category ~status in
    let milestone = milestone ~target_version in
    let closed = Status.is_closed status in
    let closed_at =
      match closed_at, closed with
      | None, true -> Some last_updated
      | None, false -> None
      | Some _ as x, _ -> x
    in
    let handler =
      match handler with
      | Some s ->
          Hashtbl.find_opt user_map s
      | None ->
          None
    in
    let issue =
      ostr "assignee" handler @@
      ostr "closed_at" closed_at @@
      [
        "title", `String summary;
        "body", `String body;
        "created_at", `String date_submitted;
        "updated_at", `String last_updated;
        "milestone", milestone;
        "closed", `Bool closed;
        "labels", `List labels;
      ]
    in
    let comments = List.map Note.to_json notes in
    `Assoc ["issue", `Assoc issue; "comments", `List comments]
end

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
          int_of_string id, user_name
      | _ ->
          failwith "Unexpected response when querying users"
    in
    let r =
      exec dbd ~f "SELECT id, username FROM mantis_user_table;"
    in
    Hashtbl.of_list r
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
    Hashtbl.of_list r
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
      Hashtbl.of_list r
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
    Hashtbl.of_list r
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
  let relationships =
    let f = function
      | [|Some source_bug_id; Some destination_bug_id|] ->
          int_of_string source_bug_id, int_of_string destination_bug_id
      | _ ->
          failwith "Unexpected response when querying relationships"
    in
    let r =
      exec dbd ~f
        "SELECT source_bug_id, destination_bug_id \
         FROM mantis_bug_relationship_table;"
    in
    let h = Hashtbl.create (List.length r) in
    List.iter (fun (id, x) -> Hashtbl.add h id x; Hashtbl.add h x id) r;
    h
  in
  let all_tags =
    let f = function
      | [|Some tag_id; Some name|] ->
          int_of_string tag_id, name
      | _ ->
          failwith "Unexpected response when querying tags"
    in
    let r =
      exec dbd ~f
        "SELECT tag_id, name FROM mantis_tag_table;"
    in
    Hashtbl.of_list r
  in
  let tags =
    let f = function
      | [|Some bug_id; Some tag_id|] ->
          int_of_string bug_id, int_of_string tag_id
      | _ ->
          failwith "Unexpected response when querying tags"
    in
    let r =
      exec dbd ~f
        "SELECT bug_id, tag_id FROM mantis_bug_tag_table;"
    in
    let r = List.map (fun (bug_id, tag_id) -> bug_id, Hashtbl.find all_tags tag_id) r in
    Hashtbl.of_list r
  in
  let query =
    "SELECT id, summary, priority, category_id, date_submitted, last_updated, \
     reporter_id, handler_id, bug_text_id, version, target_version, fixed_in_version, status, \
     resolution FROM mantis_bug_table ORDER BY id;"
  in
  let f = function
    | [|Some id; Some summary; Some priority; Some category_id;
        Some date_submitted; Some last_updated; Some reporter_id;
        Some handler_id; Some bug_text_id; Some version; Some target_version;
        Some fixed_in_version; Some status; Some resolution|] ->
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
        let resolution = Resolution.of_int (int_of_string resolution) in
        let related = List.sort Stdlib.compare (Hashtbl.find_all relationships id) in
        let tags = Hashtbl.find_all tags id in
        id,
        { Issue.id; summary; priority; category;
          date_submitted = timestamp date_submitted;
          last_updated = timestamp last_updated; reporter; handler;
          description; steps_to_reproduce; additional_information;
          version; target_version; fixed_in_version;
          notes; status; closed_at; resolution; related; tags }
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

let create_issues gh db bug_ids =
  let issues = Hashtbl.of_list (fetch db) in
  List.iter (fun id -> Curl.create_issue gh (Issue.to_json (Hashtbl.find issues id))) bug_ids

open Cmdliner

let db dbhost dbname dbport dbpwd dbuser =
  {Mysql.dbhost; dbname; dbport; dbpwd; dbuser; dbsocket = None}

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
