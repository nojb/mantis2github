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

  module L = struct
    type nonrec _t = t list

    let of_priority = function
      | Mantis.Priority.None | Normal -> []
      | Low -> [Low_priority]
      | High | Urgent -> [High_priority]
      | Immediate -> [Critical]

    let of_severity = function
      | Mantis.Severity.Feature -> [Feature]
      | Tweak | Trivial | Minor -> [Tweak]
      | Text | Major -> []
      | Crash -> [Crash]
      | Block -> [Block]

    let of_resolution = function
      | Mantis.Resolution.Open | Fixed | Reopened -> []
      | Unable_to_duplicate -> [Unable_to_reproduce]
      | Duplicate -> [Duplicate]
      | Not_a_bug -> [No_change_required]
      | Suspended -> [Suspended]
      | Wont_fix -> [Wontfix]
      | Not_fixable -> []
  end
end

let timestamp s =
  let {Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _} =
    Unix.gmtime (float_of_string s)
  in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

module Note = struct
  let migrate {Mantis.Note.reporter; text; last_modified = _; date_submitted} =
    let body =
      let reporter =
        match reporter with
        | None -> ""
        | Some s -> Printf.sprintf "*Comment author:* %s\n" s
      in
      let text = String.trim text in
      if text <> "" then reporter ^ "\n" ^ text else reporter
    in
    {Github.Issue.Comment.body; created_at = Some (timestamp date_submitted)}
end

module Issue = struct
  let body
      ~id ?(reporter = "") ~tags ~category
      ~version ~target_version ~fixed_in_version
      ~priority ~severity ~related
    =
    let combine l =
      l
      |> List.map (fun (s1, s2) -> s1, String.trim s2)
      |> List.filter (function (_, "") -> false | _ -> true)
      |> List.map (fun (s1, s2) -> Printf.sprintf "*%s:* %s" s1 s2)
      |> String.concat "\n"
    in
    let see_also =
      related
      |> List.map (fun (_, gh_id) -> Printf.sprintf "#%d" gh_id)
      |> String.concat " "
    in
    combine
      [
        "Mantis ID", string_of_int id;
        "Reporter", reporter;
        "Version", version;
        "Target version", target_version;
        "Fixed in version", fixed_in_version;
        "Category", category;
        "Priority", Mantis.Priority.to_string priority;
        "Severity", Mantis.Severity.to_string severity;
        "Tags", String.concat ", " tags;
        "Related to", see_also;
      ]

  let extra_notes
      ~created_at
      ~description
      ~steps_to_reproduce
      ~additional_information file_urls
    =
    let note title contents l =
      let title = Printf.sprintf "*%s*\n\n" title in
      match String.trim contents with
      | "" -> l
      | body ->
          let body = title ^ body in
          {Github.Issue.Comment.created_at = Some created_at; body} :: l
    in
    let file_attachments =
      match file_urls with
      | [] -> ""
      | _ :: _ ->
          file_urls
          |> List.map (fun (filename, url) -> Printf.sprintf "- [%s](%s)\n" filename url)
          |> String.concat ""
    in
    note "Bug description" description
      (note "Steps to reproduce" steps_to_reproduce
         (note "Additional information" additional_information
            (note "File attachments" file_attachments [])))

  let labels ~priority ~severity ~category:_ ~status:_ ~resolution =
    if true then [] else
      Label.L.(of_priority priority @ of_severity severity @ of_resolution resolution)
      |> List.sort_uniq Stdlib.compare
      |> List.map Label.to_string

  let milestone ~target_version:_ =
    None

  let migrate ~owner ~repo ~gh_user ~gh_ids
      {
        Mantis.Issue.id;
        summary;
        priority;
        severity;
        category;
        date_submitted;
        last_updated;
        reporter;
        handler = assignee;
        description;
        steps_to_reproduce;
        additional_information;
        version;
        target_version;
        fixed_in_version;
        notes;
        status;
        history;
        resolution;
        related;
        tags;
        files;
      }
    =
    let title = if summary = "" then "*no title*" else summary in
    let body =
      let related =
        List.filter (fun id' ->
            match gh_ids id' with
            | _ ->
                true
            | exception Not_found ->
                Printf.eprintf "WARNING: related issue %d to %d not found\n%!" id' id;
                false
          ) related
        |> List.map (fun id -> id, gh_ids id)
      in
      body
        ~id ?reporter ~tags ~category
        ~version ~target_version ~fixed_in_version
        ~priority ~severity ~related
    in
    let labels = labels ~priority ~severity ~category ~status ~resolution in
    let milestone = milestone ~target_version in
    let closed = Mantis.Status.is_closed status in
    let updated_at = timestamp last_updated in
    let created_at = timestamp date_submitted in
    let closed_at =
      match history with
      | None -> if closed then Some updated_at else None
      | Some (closed_at, st) ->
          assert (st = status);
          if Mantis.Status.is_closed status then Some (timestamp closed_at) else None
    in
    let assignee =
      match assignee with
      | Some s ->
          gh_user s
      | None ->
          None
    in
    let extra_notes urls =
      extra_notes
        ~created_at
        ~description
        ~steps_to_reproduce
        ~additional_information urls
    in
    let issue =
      {Github.Issue.Issue.title; body;
       created_at = Some created_at;
       updated_at = Some updated_at;
       assignee; milestone; closed_at;
       closed = Some closed; labels}
    in
    let comments urls = extra_notes urls @ List.map Note.migrate notes in
    let issue urls = {Github.Issue.issue; comments = comments urls} in
    let gist =
      match files with
      | [] -> None
      | _ :: _ ->
          let description =
            Printf.sprintf "https://github.com/%s/%s/issues/%d"
              owner repo (try gh_ids id with Not_found -> 1000)
          in
          Some {Github.Gist.files; description; public = true}
    in
    issue, gist
end
