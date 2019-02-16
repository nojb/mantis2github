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

module Hashtbl = struct
  include Hashtbl
  let of_list l =
    let h = Hashtbl.create (List.length l) in
    List.iter (fun (id, x) -> Hashtbl.add h id x) l;
    h
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

  let to_string = function
    | Open -> "open"
    | Fixed -> "fixed"
    | Reopened -> "reopened"
    | Unable_to_duplicate -> "unable to duplicate"
    | Not_fixable -> "not fixable"
    | Duplicate -> "duplicate"
    | Not_a_bug -> "not a bug"
    | Suspended -> "suspended"
    | Wont_fix -> "won't fix"
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
end

module Note = struct
  type t =
    {
      reporter: string option;
      text: string;
      last_modified: string;
      date_submitted: string;
    }
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
      last_status_change: (string option * string) option;
      resolution: Resolution.t;
      related: int list;
      tags: string list;
      files: (string * string) list;
    }
end

module Db = struct
  type t = Mysql.dbd

  let use db f =
    let dbd = Mysql.connect db in
    Mysql.set_charset dbd "utf8";
    match f dbd with
    | r ->
        Mysql.disconnect dbd;
        r
    | exception e ->
        Mysql.disconnect dbd;
        raise e

  let exec dbd f query =
    let f arr =
      let extract = function
        | Some x -> x
        | None -> Printf.ksprintf failwith "Unexpected result: %S" query
      in
      f (Array.map extract arr)
    in
    Mysql.map (Mysql.exec dbd query) ~f |> Hashtbl.of_list
end

let fetch dbd =
  let categories =
    let f = function
      | [|id; name|] ->
          int_of_string id, name
      | _ ->
          assert false
    in
    Db.exec dbd f "SELECT id, name FROM mantis_category_table;"
  in
  let users =
    let f = function
      | [|id; user_name|] ->
          int_of_string id, user_name
      | _ ->
          assert false
    in
    Db.exec dbd f "SELECT id, username FROM mantis_user_table;"
  in
  let texts =
    let f = function
      | [|id; description; steps_to_reproduce; additional_information|] ->
          int_of_string id, (description, steps_to_reproduce, additional_information)
      | _ ->
          assert false
    in
    Db.exec dbd f "SELECT id, description, steps_to_reproduce, additional_information \
                   FROM mantis_bug_text_table;"
  in
  let notes =
    let texts =
      let f = function
        | [|id; note|] ->
            int_of_string id, note
        | _ ->
            assert false
      in
      Db.exec dbd f "SELECT id, note FROM mantis_bugnote_text_table;"
    in
    let f = function
      | [|bug_id; reporter_id; bugnote_text_id; last_modified; date_submitted|] ->
          let reporter = Hashtbl.find_opt users (int_of_string reporter_id) in
          let text = Hashtbl.find texts (int_of_string bugnote_text_id) in
          int_of_string bug_id, {Note.reporter; text; last_modified; date_submitted}
      | _ ->
          assert false
    in
    Db.exec dbd f
      "SELECT bug_id, reporter_id, bugnote_text_id, \
       last_modified, date_submitted \
       FROM mantis_bugnote_table ORDER BY date_submitted DESC;"
  in
  let history =
    let f = function
      | [|bug_id; user_id; date_modified|] ->
          let user = Hashtbl.find_opt users (int_of_string user_id) in
          int_of_string bug_id, (user, date_modified)
      | _ ->
          assert false
    in
    Db.exec dbd f
      "SELECT bug_id, user_id, date_modified FROM mantis_bug_history_table \
       WHERE field_name = 'status' ORDER BY date_modified ASC;"
  in
  let files =
    let f = function
      | [|bug_id; filename; content|] ->
          int_of_string bug_id, (filename, content)
      | _ ->
          assert false
    in
    Db.exec dbd f
      "SELECT bug_id, filename, content FROM mantis_bug_file_table \
       ORDER BY date_added DESC;"
  in
  let relationships =
    let f = function
      | [|source_bug_id; destination_bug_id|] ->
          int_of_string source_bug_id, int_of_string destination_bug_id
      | _ ->
          assert false
    in
    let h =
      Db.exec dbd f
        "SELECT source_bug_id, destination_bug_id \
         FROM mantis_bug_relationship_table;"
    in
    let h' = Hashtbl.create (Hashtbl.length h) in
    Hashtbl.iter (fun id x -> Hashtbl.add h' id x; Hashtbl.add h' x id) h;
    h'
  in
  let tags =
    let all_tags =
      let f = function
        | [|tag_id; name|] ->
            int_of_string tag_id, name
        | _ ->
            assert false
      in
      Db.exec dbd f "SELECT id, name FROM mantis_tag_table;"
    in
    let f = function
      | [|bug_id; tag_id|] ->
          int_of_string bug_id, Hashtbl.find all_tags (int_of_string tag_id)
      | _ ->
          assert false
    in
    Db.exec dbd f "SELECT bug_id, tag_id FROM mantis_bug_tag_table;"
  in
  let query =
    "SELECT id, summary, priority, severity, category_id, date_submitted, last_updated, \
     reporter_id, handler_id, bug_text_id, version, target_version, fixed_in_version, status, \
     resolution FROM mantis_bug_table ORDER BY id;"
  in
  let f = function
    | [|id; summary; priority; severity; category_id;
        date_submitted; last_updated; reporter_id;
        handler_id; bug_text_id; version; target_version;
        fixed_in_version; status; resolution|] ->
        let id = int_of_string id in
        let category = Hashtbl.find categories (int_of_string category_id) in
        let reporter = Hashtbl.find_opt users (int_of_string reporter_id) in
        let handler = Hashtbl.find_opt users (int_of_string handler_id) in
        let description, steps_to_reproduce, additional_information =
          Hashtbl.find texts (int_of_string bug_text_id)
        in
        let notes = Hashtbl.find_all notes id in
        let status = Status.of_int (int_of_string status) in
        let last_status_change = Hashtbl.find_opt history id in
        let resolution = Resolution.of_int (int_of_string resolution) in
        let related = List.sort Stdlib.compare (Hashtbl.find_all relationships id) in
        let tags = Hashtbl.find_all tags id in
        let priority = Priority.of_int (int_of_string priority) in
        let severity = Severity.of_int (int_of_string severity) in
        let files = Hashtbl.find_all files id in
        id,
        { Issue.id; summary; priority; severity; category;
          date_submitted; last_updated; reporter; handler;
          description; steps_to_reproduce; additional_information;
          version; target_version; fixed_in_version;
          notes; status; last_status_change; resolution; related; tags; files }
    | _ ->
        assert false
  in
  Db.exec dbd f query
