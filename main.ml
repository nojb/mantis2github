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

let oint s o l =
  match o with
  | Some x -> (s, `Int x) :: l
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

  let curl meth ?(headers = []) ?data:arg ?(params = []) {owner; repo; token} route =
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
      match arg with
      | None -> ""
      | Some data ->
          (* Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) data; *)
          let filename, oc = Filename.open_temp_file "curl" "data" in
          set_binary_mode_out oc true;
          Yojson.Basic.pretty_to_channel ~std:true oc data;
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
    (* Printf.eprintf "+ %s\n%!" cmd; *)
    let tmp = Filename.temp_file "curl" "out" in
    assert (Sys.command (Printf.sprintf "%s > %s" cmd tmp) = 0);
    let ic = open_in_bin tmp in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let json = Yojson.Basic.from_string s in
    (* Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json; *)
    match J.member "message" json with
    | `String _ ->
        let json = ["cmd", `String cmd; "res", json] in
        let json = match arg with None -> json | Some arg -> ("arg", arg) :: json in
        Error (`Assoc json)
    | _ ->
        Ok json

  let get ?headers ?params gh route = curl "GET" ?headers ?params gh route
  let post ?headers ?data gh route = curl "POST" ?headers ?data gh route

  let list_milestones gh =
    let f json =
      let title = json |> J.member "title" |> J.to_string in
      let number = json |> J.member "number" |> J.to_int in
      Some (title, number)
    in
    match get ~params:["state", "all"] gh "milestones" with
    | Error json ->
        Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json;
        None
    | Ok json ->
        Some (json |> J.to_list |> J.filter_map f)

  let is_imported gh id =
    match get gh (Printf.sprintf "import/issues/%d" id) with
    | Error _ as x -> Some x
    | Ok json ->
        begin match json |> J.member "status" |> J.to_string with
        | "imported" ->
            let id =
              json
              |> J.member "issue_url"
              |> J.to_string
              |> String.split_on_char '/'
              |> List.rev
              |> List.hd
              |> int_of_string
            in
            Some (Ok id)
        | "failed" ->
            Some (Error json)
        | _ ->
            None
        end

  let start_import gh json =
    match post ~data:json gh "import/issues" with
    | Ok json -> Ok (json |> J.member "id" |> J.to_int)
    | Error _ as x -> x

  let create_issue gh json =
    match start_import gh json with
    | Error json ->
        Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json;
        None
    | Ok id ->
        let rec loop n =
          match is_imported gh id with
          | Some (Ok id) -> Some id
          | Some (Error json_err) ->
              let json = `Assoc ["issue", json; "error", json_err] in
              Printf.eprintf "%a\n%!" (Yojson.Basic.pretty_to_channel ~std:true) json;
              None
          | None ->
              Unix.sleep n; loop (2 * n)
        in
        loop 1
end

module Labels = struct
  type t =
    | Duplicate
    | No_change_required
    | Unable_to_reproduce
    | Wontfix
    | Critical
    | High_priority
    | Low_priority
    | Suspended

  let to_string = function
    | Duplicate -> "duplicate"
    | No_change_required -> "no change required"
    | Unable_to_reproduce -> "unable to reproduce"
    | Wontfix -> "wontfix"
    | Critical -> "critical"
    | High_priority -> "high priority"
    | Low_priority -> "low priority"
    | Suspended -> "suspended"
end

module Priority = struct
  type t =
    | None
    | Low
    | Normal
    | High
    | Urgent
    | Immediate

  let of_int = function
    | 10 -> None
    | 20 -> Low
    | 30 -> Normal
    | 40 -> High
    | 50 -> Urgent
    | 60 -> Immediate
    | n -> Printf.ksprintf failwith "Unexpected priority code: %d" n

  let to_string = function
    | None -> ""
    | Low -> "low"
    | Normal -> "normal"
    | High -> "high"
    | Urgent -> "urgent"
    | Immediate -> "immediate"

  let to_labels = function
    | None | Normal -> []
    | Low -> [Labels.Low_priority]
    | High | Urgent -> [Labels.High_priority]
    | Immediate -> [Labels.Critical]
end

module Severity = struct
  type t =
    | Feature
    | Trivial
    | Text
    | Tweak
    | Minor
    | Major
    | Crash
    | Block

  let of_int = function
    | 10 -> Feature
    | 20 -> Trivial
    | 30 -> Text
    | 40 -> Tweak
    | 50 -> Minor
    | 60 -> Major
    | 70 -> Crash
    | 80 -> Block
    | n -> Printf.ksprintf failwith "Unexpected severity code: %d" n

  let to_string = function
    | Feature -> "feature"
    | Trivial -> "trivial"
    | Text -> "text"
    | Tweak -> "tweak"
    | Minor -> "minor"
    | Major -> "major"
    | Crash -> "crash"
    | Block -> "block"

  let to_labels = function
    | Feature | Trivial | Tweak | Minor -> Labels.[Low_priority]
    | Text -> []
    | Major | Crash -> Labels.[High_priority]
    | Block -> Labels.[Critical]
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

  let to_labels = function
    | Open | Fixed | Reopened -> []
    | Unable_to_duplicate -> [Labels.Unable_to_reproduce]
    | Duplicate -> [Labels.Duplicate]
    | Not_a_bug -> [Labels.No_change_required]
    | Suspended -> [Labels.Suspended]
    | Wont_fix -> [Labels.Wontfix]
    | Not_fixable -> []
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

let badd buf title s =
  let fence = String.make 6 '`' in
  let s = String.trim s in
  if s <> "" then Printf.bprintf buf "**%s**\n%s\n%s\n%s\n" title fence s fence

module Note = struct
  type t =
    {
      reporter: string option;
      text: string;
      last_modified: string;
      date_submitted: string;
    }

  let to_json {reporter; text; last_modified = _; date_submitted} =
    let reporter = match reporter with None -> "" | Some s -> s in
    let text =
      let buf = Buffer.create 101 in
      badd buf "Reporter" reporter;
      badd buf "Body" text;
      Buffer.contents buf
    in
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
      priority: Priority.t;
      severity: Severity.t;
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

  let body ~id ?(reporter = "") ~tags ~category ~version ~target_version ~fixed_in_version
      ~priority ~severity
      ~description ~steps_to_reproduce ~additional_information ~related:_
    =
    let buf = Buffer.create 101 in
    let info =
      let combine l =
        let l = List.map (fun (s1, s2) -> (s1, String.trim s2)) l in
        let l = List.filter (function (_, "") -> false | _ -> true) l in
        String.concat "\n" (List.map (fun (s1, s2) -> s1 ^ ": " ^ s2) l)
      in
      combine
        [ "ID", Printf.sprintf "%07d" id;
          "Reporter", reporter;
          "Version", version;
          "Target version", target_version;
          "Fixed in version", fixed_in_version;
          "Category", category;
          "Priority", Priority.to_string priority;
          "Severity", Severity.to_string severity;
          "Tags", String.concat ", " tags ];
    in
    badd buf "Original bug information" info;
    badd buf "Description" description;
    badd buf "Steps to reproduce" steps_to_reproduce;
    badd buf "Additional information" additional_information;
    Buffer.contents buf

  let labels ~priority ~severity ~category:_ ~status:_ ~resolution =
    let l =
      Priority.to_labels priority @
      Severity.to_labels severity @
      Resolution.to_labels resolution
    in
    List.sort_uniq Stdlib.compare l |> List.map Labels.to_string

  let milestone ~target_version:_ =
    None

  let to_json ?assignee
      {
        id;
        summary;
        priority;
        severity;
        category;
        date_submitted;
        last_updated;
        reporter;
        handler;
        description;
        steps_to_reproduce;
        additional_information;
        version;
        target_version;
        fixed_in_version;
        notes;
        status;
        closed_at;
        resolution;
        related;
        tags;
      }
    =
    let body =
      body ~id ?reporter ~tags ~category ~version ~target_version ~fixed_in_version
        ~priority ~severity
        ~description ~steps_to_reproduce ~additional_information ~related
    in
    let labels = labels ~priority ~severity ~category ~status ~resolution in
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
    let handler =
      match handler with
      | Some _ -> assignee
      | None -> None
    in
    let issue =
      ostr "assignee" handler @@
      ostr "closed_at" closed_at @@
      oint "milestone" milestone @@
      [
        "title", `String summary;
        "body", `String body;
        "created_at", `String date_submitted;
        "updated_at", `String last_updated;
        "closed", `Bool closed;
        "labels", `List (List.map (fun s -> `String s) labels);
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
    let r = exec dbd ~f "SELECT id, name FROM mantis_tag_table;" in
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
    "SELECT id, summary, priority, severity, category_id, date_submitted, last_updated, \
     reporter_id, handler_id, bug_text_id, version, target_version, fixed_in_version, status, \
     resolution FROM mantis_bug_table ORDER BY id;"
  in
  let f = function
    | [|Some id; Some summary; Some priority; Some severity; Some category_id;
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
        let priority = Priority.of_int (int_of_string priority) in
        let severity = Severity.of_int (int_of_string severity) in
        id,
        { Issue.id; summary; priority; severity; category;
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

let extract db = function
  | [] ->
      let f (_, issue) =
        let json = Issue.to_json issue in
        Printf.printf "%a\n" (Yojson.pretty_to_channel ~std:true) json
      in
      List.iter f (fetch db)
  | bug_ids ->
      let issues = Hashtbl.of_list (fetch db) in
      List.iter (fun id ->
          match Hashtbl.find_opt issues id with
          | None ->
              Printf.eprintf "No Mantis issue found with id %d\n%!" id
          | Some issue ->
              let json = Issue.to_json issue in
              Printf.printf "%a\n%!" (Yojson.pretty_to_channel ~std:true) json
        ) bug_ids

let milestones gh =
  match Curl.list_milestones gh with
  | Some l ->
      List.iter print_endline (List.map fst l)
  | None ->
      ()

let create_issues gh db bug_ids =
  let issues = Hashtbl.of_list (fetch db) in
  List.iter (fun id ->
      ignore (Curl.create_issue gh (Issue.to_json (Hashtbl.find issues id)))
    ) bug_ids

let migrate gh db assignee from nmax =
  let issues = Hashtbl.of_list (fetch db) in
  let n = Hashtbl.length issues in
  let rec loop total count idx =
    if count >= min n nmax then ()
    else begin
      match Hashtbl.find_opt issues idx with
      | None ->
          loop total count (succ idx)
      | Some issue ->
          let starttime = Unix.gettimeofday () in
          let res = Curl.create_issue gh (Issue.to_json ?assignee issue) in
          let endtime = Unix.gettimeofday () in
          let delta = endtime -. starttime in
          let total = total +. delta in
          let id =
            match res with
            | Some id -> string_of_int id
            | None -> "ERR"
          in
          let count = succ count in
          Printf.printf "%4d %4s %6.1f %6.1f %6.1f\n%!" issue.Issue.id id delta (total /. float count) total;
          loop total count (succ idx)
    end
  in
  loop 0. 0 from

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
  let github owner repo token = {owner; repo; token} in
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
  Term.(const migrate $ github_t $ db_t $ force_assignee_t $ from_t $ nmax_t),
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
