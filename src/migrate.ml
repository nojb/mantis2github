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
    (* | No_change_required *)
    | Wontfix
    | High_priority
    | Suspended

    | Caml_light
    | Caml_idl
    | Camlp4
    | Ocamlbuild
    | Backend
    | Compiler_driver
    | Configure_related
    | Documentation
    | Dynlink
    | Emacs_mode
    | Language_features
    | Lexing_and_parsing
    | Middleend
    | Ocamldoc
    | Otherlibs
    | Platform_support
    | Runtime_C_interface
    | Stdlib
    | Threads
    | Tools
    | Toplevel
    | Typing
    | Web_site
    | Junior_job

  let to_string = function
    | Duplicate -> "duplicate"
    (* | No_change_required -> "no change required" *)
    | Wontfix -> "wontfix"
    | High_priority -> "high-priority"
    | Suspended -> "suspended"

    | Caml_light -> "caml-light"
    | Caml_idl -> "camlidl"
    | Camlp4 -> "camlp4"
    | Ocamlbuild -> "ocamlbuild"
    | Backend -> "back-end"
    | Compiler_driver -> "compiler-driver"
    | Configure_related -> "configure-related"
    | Documentation -> "documentation"
    | Dynlink -> "dynlink"
    | Emacs_mode -> "emacs-mode"
    | Language_features -> "language-features"
    | Lexing_and_parsing -> "lexing-and-parsing"
    | Middleend -> "middle-end"
    | Ocamldoc -> "ocamldoc"
    | Otherlibs -> "otherlibs"
    | Platform_support -> "platform-support"
    | Runtime_C_interface
    | Stdlib -> "stdlib"
    | Threads -> "threads"
    | Tools -> "tools"
    | Toplevel -> "toplevel"
    | Typing -> "typing"
    | Web_site -> "website"
    | Junior_job -> "junior-job"

  module L = struct
    type nonrec _t = t list

    let of_priority = function
      | Mantis.Priority.None | Normal | Low -> []
      | High | Urgent | Immediate -> [High_priority]

    let of_severity _ = []

    let of_resolution = function
      | Mantis.Resolution.Open | Fixed | Reopened
      | Unable_to_duplicate -> []
      | Duplicate -> [Duplicate]
      | Not_a_bug -> [] (* [No_change_required] *)
      | Suspended -> [Suspended]
      | Wont_fix -> [Wontfix]
      | Not_fixable -> []

    let of_category = function
      | "-for Caml light use https://github.com/camllight/camllight/issues" -> [Caml_light]
      | "-for CamlIDL use https://github.com/xavierleroy/camlidl/issues" -> [Caml_idl]
      | "-for Camlp4 use https://github.com/ocaml/camlp4/issues" -> [Camlp4]
      | "-for ocamlbuild use https://github.com/ocaml/ocamlbuild/issues" -> [Ocamlbuild]
      | "back end (clambda to assembly)" -> [Backend]
      | "compiler driver" -> [Compiler_driver]
      | "configure and build/install" -> [Configure_related]
      | "documentation" -> [Documentation]
      | "dynlink and natdynlink" -> [Dynlink]
      | "emacs mode" -> [Emacs_mode]
      | "language features" -> [Language_features]
      | "lexing and parsing" -> [Lexing_and_parsing]
      | "middle end (typedtree to clambda)" -> [Middleend]
      | "ocamldoc" -> [Ocamldoc]
      | "otherlibs" -> [Otherlibs]
      | "platform support (windows, cross-compilation, etc)" -> [Platform_support]
      | "runtime system and C interface" -> [Runtime_C_interface]
      | "standard library" -> [Stdlib]
      | "threads" -> [Threads]
      | "tools (ocaml{lex,yacc,dep,debug,...})" -> [Tools]
      | "toplevel" -> [Toplevel]
      | "typing" -> [Typing]
      | "web site" -> [Web_site]
      | _ -> []

    let of_tag = function
      | "caml-mode" -> [Emacs_mode]
      | "junior_job" -> [Junior_job]
      | "manual" -> [Documentation]
      | "ocamldoc" -> [Ocamldoc]
      | "recmod" -> [Typing]
      | "typing" -> [Typing]
      | _ -> []
  end
end

let timestamp n =
  let {Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _} =
    Unix.gmtime (float n)
  in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let gpr_re =
  Re.(word (seq [str "GPR#"; group (rep1 digit)]) |> compile)

let mpr_re =
  Re.(word (seq [opt (char 'M'); str "PR#"; group (rep1 digit)]) |> compile)

let add_pr_links ~owner:_ ~repo:_ ~gh_ids s =
  let f g = Printf.sprintf "#%s" (Re.Group.get g 1) in
  let s = Re.replace gpr_re ~f s in
  let f g = Printf.sprintf "#%d" (Re.Group.get g 1 |> int_of_string |> gh_ids) in
  Re.replace mpr_re ~f s

module Note = struct
  let migrate ~owner ~repo ~gh_ids
      {Mantis.Note.reporter; text; last_modified = _; date_submitted}
    =
    let body =
      let reporter =
        match reporter with
        | None -> ""
        | Some s -> Printf.sprintf "**Comment author:** %s\n" s
      in
      let text = String.trim text |> add_pr_links ~owner ~repo ~gh_ids in
      if text <> "" then reporter ^ "\n" ^ text else reporter
    in
    {Github.Issue.Comment.body; created_at = Some (timestamp date_submitted)}
end

module Issue = struct
  let pr ~owner:_ ~repo:_ (_id, gh_id) =
    Printf.sprintf "#%d" gh_id

  let body ~owner ~repo ~gh_ids file_urls
      ({
        Mantis.Issue.id;
        summary = _;
        priority;
        severity;
        category;
        date_submitted = _;
        last_updated = _;
        reporter;
        handler;
        description;
        steps_to_reproduce;
        additional_information;
        version;
        target_version;
        fixed_in_version;
        notes = _;
        status;
        last_status_change;
        resolution;
        duplicate_of;
        has_duplicate;
        related_to;
        child_of;
        parent_of;
        tags;
        os;
        os_build;
        platform;
        files = _;
      } as _issue)
    =
    let combine l =
      l
      |> List.map (fun (s1, s2) -> s1, String.trim s2)
      |> List.filter (function (_, "") -> false | _ -> true)
      |> List.map (fun (s1, s2) -> Printf.sprintf "**%s:** %s\n" s1 s2)
      |> String.concat ""
    in
    let see_also l =
      l
      |> List.map (fun id -> pr ~owner ~repo (id, gh_ids id))
      |> String.concat " "
    in
    let status =
      match last_status_change with
      | None ->
          Mantis.Status.to_string status
      | Some (Some s1, s2) ->
          Printf.sprintf "%s (by %s on %s)"
            (Mantis.Status.to_string status) s1 (timestamp s2)
      | Some (None, s) ->
          Printf.sprintf "%s (on %s)"
            (Mantis.Status.to_string status) (timestamp s)
    in
    let reporter = match reporter with None -> "" | Some s -> s in
    let handler = match handler with None -> "" | Some s -> s in
    let note title contents =
      match String.trim contents with
      | "" -> ""
      | body ->
          let title = Printf.sprintf "\n## %s\n\n" title in
          String.concat "" [title; body; "\n"]
    in
    let file_attachments =
      match file_urls with
      | [] -> ""
      | _ :: _ ->
          file_urls
          |> List.map (fun (filename, url) -> Printf.sprintf "- [%s](%s)\n" filename url)
          |> String.concat ""
    in
    (* let comment s = "\n<!-- ocaml =\n" ^ s ^ "\n-->\n" in *)
    String.concat ""
      [
        combine
          [
            "Original bug ID", Printf.sprintf "PR#%d" id;
            "Reporter", reporter;
            "Assigned to", handler;
            "Status", status;
            "Resolution", Mantis.Resolution.to_string resolution;
            "Priority", Mantis.Priority.to_string priority;
            "Severity", Mantis.Severity.to_string severity;
            "Platform", platform;
            "OS", os;
            "OS Version", os_build;
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
          ];
        note "Bug description" description;
        note "Steps to reproduce" steps_to_reproduce;
        note "Additional information" additional_information;
        note "File attachments" file_attachments;
        (* comment (Yojson.Basic.pretty_to_string ~std:true (Mantis.Issue.to_json issue)) *)
      ]

  let labels ~priority ~severity ~category ~tags ~status:_ ~resolution ~duplicate_of =
    let dup = if duplicate_of <> [] then [Label.Duplicate] else [] in
    let tags = List.concat (List.map Label.L.of_tag tags) in
    Label.L.(of_priority priority @ of_severity severity @ of_resolution resolution @ of_category category @ tags @ dup)
    |> List.sort_uniq Stdlib.compare
    |> List.map Label.to_string

  let milestone ~target_version:_ =
    None

  let migrate (owner, repo) ~gh_ids
      ({
        Mantis.Issue.id;
        summary;
        priority;
        severity;
        category;
        date_submitted;
        last_updated;
        reporter = _;
        handler = assignee;
        description = _;
        steps_to_reproduce = _;
        additional_information = _;
        version = _;
        target_version;
        fixed_in_version = _;
        notes;
        status;
        last_status_change;
        resolution;
        duplicate_of;
        has_duplicate = _;
        related_to = _;
        child_of = _;
        parent_of = _;
        tags;
        os = _;
        os_build = _;
        platform = _;
        files;
      } as issue)
    =
    let title = if summary = "" then "*no title*" else summary in
    let body urls = body ~owner ~repo ~gh_ids urls issue in
    let labels = labels ~priority ~severity ~category ~tags ~status ~resolution ~duplicate_of in
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
      | "ocaml", Some s -> mantis2gh s
      | _ -> None
    in
    let issue urls =
      {Github.Issue.Issue.title; body = body urls;
       created_at = Some created_at;
       updated_at = Some updated_at;
       assignee; milestone; closed_at;
       closed = Some closed; labels}
    in
    let comments = List.map (Note.migrate ~owner ~repo ~gh_ids) notes in
    let issue urls = {Github.Issue.issue = issue urls; comments} in
    let gist =
      match files with
      | [] -> None
      | _ :: _ ->
          let description =
            Printf.sprintf "https://github.com/%s/%s/issues/%d"
              owner repo (gh_ids id)
          in
          Some {Github.Gist.files; description; public = true}
    in
    issue, gist
end
