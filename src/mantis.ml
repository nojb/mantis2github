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

let str s1 s2 l =
  match s2 with
  | None -> l
  | Some s2 -> (s1, `String s2) :: l

let bool s1 s2 l =
  match s2 with
  | None -> l
  | Some s2 -> (s1, `Bool s2) :: l

let int s1 s2 l =
  match s2 with
  | None -> l
  | Some s2 -> (s1, `Int s2) :: l

module Label = struct
  type t =
    | Duplicate
    | No_change_required
    | Unable_to_reproduce
    | Wontfix
    | Critical
    | High_priority
    | Low_priority
    | Suspended
    | Feature
    | Tweak
    | Crash
    | Block

  let to_string = function
    | Duplicate -> "duplicate"
    | No_change_required -> "no change required"
    | Unable_to_reproduce -> "unable to reproduce"
    | Wontfix -> "wontfix"
    | Critical -> "critical"
    | High_priority -> "high priority"
    | Low_priority -> "low priority"
    | Suspended -> "suspended"
    | Feature -> "feature"
    | Tweak -> "tweak"
    | Crash -> "crash"
    | Block -> "block"
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
    | Low -> [Label.Low_priority]
    | High | Urgent -> [Label.High_priority]
    | Immediate -> [Label.Critical]
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
    | Feature -> Label.[Feature]
    | Tweak | Trivial | Minor -> Label.[Tweak]
    | Text | Major -> []
    | Crash -> Label.[Crash]
    | Block -> Label.[Block]

  let to_json x =
    `String (to_string x)
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
    | Unable_to_duplicate -> [Label.Unable_to_reproduce]
    | Duplicate -> [Label.Duplicate]
    | Not_a_bug -> [Label.No_change_required]
    | Suspended -> [Label.Suspended]
    | Wont_fix -> [Label.Wontfix]
    | Not_fixable -> []

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

  let to_json x =
    `String (to_string x)
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

  let to_json {reporter; text; last_modified; date_submitted} =
    let l = ("date_submitted", `String date_submitted) :: [] in
    let l = ("last_modified", `String last_modified) :: l in
    let l = ("text", `String text) :: l in
    let l = str "reporter" reporter l in
    `Assoc l
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

  let to_json
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
    let l = [] in
    let l = ("tags", `List (List.map (fun s -> `String s) tags)) :: l in
    let l = ("related", `List (List.map (fun n -> `Int n) related)) :: l in
    let l = ("resolution", Resolution.to_json resolution) :: l in
    let l = str "closed_at" closed_at l in
    let l = ("status", Status.to_json status) :: l in
    let l = ("notes", `List (List.map Note.to_json notes)) :: l in
    let l = ("fixed_in_version", `String fixed_in_version) :: l in
    let l = ("target_version", `String target_version) :: l in
    let l = ("version", `String version) :: l in
    let l = ("additional_information", `String additional_information) :: l in
    let l = ("steps_to_reproduce", `String steps_to_reproduce) :: l in
    let l = ("description", `String description) :: l in
    let l = str "handler" handler l in
    let l = str "reporter" reporter l in
    let l = ("last_updated", `String last_updated) :: l in
    let l = ("date_submitted", `String date_submitted) :: l in
    let l = ("category", `String category) :: l in
    let l = ("severity", `String (Severity.to_string severity)) :: l in
    let l = ("priority", `String (Priority.to_string priority)) :: l in
    let l = ("summary", `String summary) :: l in
    let l = ("id", `Int id) :: l in
    `Assoc l
end

let exec dbd ~f query =
  let f arr =
    let extract = function
      | Some x -> x
      | None -> Printf.ksprintf failwith "Unexpected result: %S" query
    in
    f (Array.map extract arr)
  in
  Mysql.map (Mysql.exec dbd query) ~f

let timestamp s =
  let {Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _} =
    Unix.gmtime (float_of_string s)
  in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let main dbd =
  let categories =
    let r =
      let f = function
        | [|id; name|] ->
            int_of_string id, name
        | _ ->
            assert false
      in
      exec dbd ~f "SELECT id, name FROM mantis_category_table;"
    in
    Hashtbl.of_seq (List.to_seq r)
  in
  let users =
    let r =
      let f = function
        | [|id; user_name|] ->
            int_of_string id, user_name
        | _ ->
            assert false
      in
      exec dbd ~f "SELECT id, username FROM mantis_user_table;"
    in
    Hashtbl.of_list r
  in
  let texts =
    let r =
      let f = function
        | [|id; description; steps_to_reproduce; additional_information|] ->
            int_of_string id, (description, steps_to_reproduce, additional_information)
        | _ ->
            assert false
      in
      exec dbd ~f "SELECT id, description, steps_to_reproduce, additional_information \
                   FROM mantis_bug_text_table;"
    in
    Hashtbl.of_list r
  in
  let notes =
    let texts =
      let r =
        let f = function
          | [|id; note|] ->
              int_of_string id, note
          | _ ->
              assert false
        in
        exec dbd ~f "SELECT id, note FROM mantis_bugnote_text_table;"
      in
      Hashtbl.of_list r
    in
    let r =
      let f = function
        | [|bug_id; reporter_id; bugnote_text_id; last_modified; date_submitted|] ->
            let reporter = Hashtbl.find_opt users (int_of_string reporter_id) in
            let text = Hashtbl.find texts (int_of_string bugnote_text_id) in
            let last_modified = timestamp last_modified in
            let date_submitted = timestamp date_submitted in
            int_of_string bug_id, {Note.reporter; text; last_modified; date_submitted}
        | _ ->
            assert false
      in
      exec dbd ~f "SELECT bug_id, reporter_id, bugnote_text_id, \
                   last_modified, date_submitted \
                   FROM mantis_bugnote_table ORDER BY date_submitted DESC;"
    in
    Hashtbl.of_list r
  in
  let statuses =
    let r =
      let f = function
        | [|bug_id; date_modified; new_value|] ->
            int_of_string bug_id,
            (timestamp date_modified, Status.of_int (int_of_string new_value))
        | _ ->
            assert false
      in
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
      | [|source_bug_id; destination_bug_id|] ->
          int_of_string source_bug_id, int_of_string destination_bug_id
      | _ ->
          assert false
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
    let r =
      let f = function
        | [|tag_id; name|] ->
            int_of_string tag_id, name
        | _ ->
            assert false
      in
      exec dbd ~f "SELECT id, name FROM mantis_tag_table;"
    in
    Hashtbl.of_list r
  in
  let tags =
    let r =
      let f = function
        | [|bug_id; tag_id|] ->
            int_of_string bug_id, int_of_string tag_id
        | _ ->
            assert false
      in
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
        assert false
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
