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

let mantis2gh = function
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

let mantis2gh s =
  try Some (mantis2gh s) with Not_found -> None

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
  let pr ~owner ~repo (id, gh_id) =
    Printf.sprintf "[PR#%d](%s/%s#%d)" id owner repo gh_id

  let body
      ~owner ~repo
      ~id ?(reporter = "") ~tags ~category
      ~version ~target_version ~fixed_in_version
      ~status ~priority ~severity ~resolution ~last_status_change
      ~duplicate_of ~has_duplicate ~related_to ~child_of ~parent_of
    =
    let combine l =
      l
      |> List.map (fun (s1, s2) -> s1, String.trim s2)
      |> List.filter (function (_, "") -> false | _ -> true)
      |> List.map (fun (s1, s2) -> Printf.sprintf "*%s:* %s" s1 s2)
      |> String.concat "\n"
    in
    let see_also l =
      l
      |> List.map (pr ~owner ~repo)
      |> String.concat " "
    in
    let status =
      match last_status_change with
      | None -> Mantis.Status.to_string status
      | Some (Some s1, s2) ->
          Printf.sprintf "%s (by %s on %s)"
            (Mantis.Status.to_string status) s1 (timestamp s2)
      | Some (None, s) ->
          Printf.sprintf "%s (on %s)"
            (Mantis.Status.to_string status) (timestamp s)
    in
    combine
      [
        "Mantis ID", string_of_int id;
        "Reporter", reporter;
        "Status", status;
        "Resolution", Mantis.Resolution.to_string resolution;
        "Priority", Mantis.Priority.to_string priority;
        "Severity", Mantis.Severity.to_string severity;
        "Version", version;
        "Target version", target_version;
        "Fixed in version", fixed_in_version;
        "Category", category;
        "Tags", String.concat ", " tags;
        "Duplicate of", see_also duplicate_of;
        "Has duplicate", see_also has_duplicate;
        "Related to", see_also related_to;
        "Child of", see_also child_of;
        "Parent of", see_also parent_of;
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

  let migrate (owner, repo) ~gh_ids
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
        last_status_change;
        resolution;
        duplicate_of;
        has_duplicate;
        related_to;
        child_of;
        parent_of;
        tags;
        files;
      }
    =
    let title = if summary = "" then "*no title*" else summary in
    let body =
      let gh id = id, gh_ids id in
      let duplicate_of = List.map gh duplicate_of in
      let has_duplicate = List.map gh has_duplicate in
      let related_to = List.map gh related_to in
      let child_of = List.map gh child_of in
      let parent_of = List.map gh parent_of in
      body
        ~owner ~repo
        ~id ?reporter ~tags ~category
        ~version ~target_version ~fixed_in_version
        ~status ~priority ~severity ~resolution ~last_status_change
        ~duplicate_of ~has_duplicate ~related_to ~child_of ~parent_of
    in
    let labels = labels ~priority ~severity ~category ~status ~resolution in
    let milestone = milestone ~target_version in
    let closed = Mantis.Status.is_closed status in
    let updated_at = timestamp last_updated in
    let created_at = timestamp date_submitted in
    let closed_at =
      match last_status_change with
      | None -> if closed then Some updated_at else None
      | Some (_, s) -> if Mantis.Status.is_closed status then Some (timestamp s) else None
    in
    let assignee =
      match owner, assignee with
      | "owner", Some s -> mantis2gh s
      | _ -> None
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
