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

module List = struct
  include List

  let rec drop n = function
    | [] -> []
    | _ :: l when n > 0 -> drop (pred n) l
    | _ as l -> l

  let rec truncate n = function
    | [] -> []
    | x :: l when n > 0 -> x :: truncate (pred n) l
    | _ -> []
end

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
      dbname = Some "db";
    }
  in
  Mantis.Db.use db Mantis.fetch

let extract () =
  let f _ issue =
    let json = Mantis.Issue.to_json issue in
    Printf.printf "%a\n" (Yojson.Basic.pretty_to_channel ~std:true) json
  in
  Hashtbl.iter f issues

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

let output_assignment oc a =
  List.iter (fun (id, gh_id) -> Printf.fprintf oc "%4d %4d\n" id gh_id) a

type gh =
  {
    token: string;
    owner: string;
    repo: string;
  }

(* let get_next verbose {token; owner; repo} = *)
(*   match Github.Issue.count ~verbose ~token ~owner ~repo () with *)
(*   | None -> *)
(*       failwith "Could not count issues, cannot proceed!" *)
(*   | Some n -> *)
(*       succ n *)

let import verbose token repo txt next =
  let a = Hashtbl.keys issues |> compute_assignment next in
  begin match txt with
  | None -> ()
  | Some txt ->
      with_out txt (fun oc -> output_assignment oc a)
  end;
  let gh_ids = Hashtbl.find (Hashtbl.of_assoc a) in
  let rec f id =
    let issue = Hashtbl.find issues id in
    let gh_issue, gh_gist = Migrate.Issue.migrate repo ~gh_ids issue in
    let gist_urls =
      match gh_gist with
      | None -> []
      | Some gist -> Github.Gist.create ~verbose ?token gist
    in
    let iid = Github.Issue.import ~verbose ?token repo (gh_issue gist_urls) in
    let rec loop sleep =
      Unix.sleep sleep;
      match Github.Issue.check_imported ~verbose ?token repo iid with
      | Pending ->
        loop (2 * sleep)
      | Failed ->
        failwith "import failed"
      | Imported gh_id' ->
        let gh_id = gh_ids id in
        if gh_id <> gh_id' then
          Printf.ksprintf failwith
            "Github ID mismatch! (id=%d,gh_id=%d,gh_id'=%d)" id gh_id gh_id';
        Printf.printf "%4d => %4d\n%!" id gh_id
    in
    loop 1;
    f (succ id)
  in
  f 1

let import_one verbose token repo id =
  let a = Hashtbl.keys issues |> compute_assignment 1 in
  let gh_ids = Hashtbl.find (Hashtbl.of_assoc a) in
  let issue = Hashtbl.find issues id in
  let gh_issue, gh_gist = Migrate.Issue.migrate repo ~gh_ids issue in
  let gist_urls =
    match gh_gist with
    | None -> []
    | Some gist -> Github.Gist.create ~verbose ?token gist
  in
  let iid = Github.Issue.import ~verbose ?token repo (gh_issue gist_urls) in
  let rec loop sleep =
    Unix.sleep sleep;
    match Github.Issue.check_imported ~verbose ?token repo iid with
    | Pending ->
        loop (2 * sleep)
    | Failed ->
        failwith "Failed"
    | Imported gh_id ->
        Printf.printf "%4d => %4d\n%!" id gh_id
  in
  loop 1

let import verbose token repo txt =
  import verbose token repo txt 1 (* (get_next verbose gh) *)

open Cmdliner

let verbose_t =
  let doc = "Be verbose." in
  Arg.(value & flag & info ["verbose"; "v"] ~doc)

let extract_cmd =
  let doc = "Extract Mantis into JSON" in
  Term.(const extract $ const ()), Term.info "extract" ~doc

let token_t =
  let doc = "Github token." in
  Arg.(value & opt (some string) None & info ["token"] ~doc ~docv:"TOKEN")

let repo_t =
  let doc = "Github repository." in
  Arg.(required & pos 0 (some (pair ~sep:'/' string string)) None & info [] ~doc ~docv:"OWNER/REPO")

let o_t =
  let doc = "Output assignment." in
  Arg.(value & opt (some string) None & info ["o"] ~doc)

let import_cmd =
  let doc = "Import issues." in
  Term.(const import $ verbose_t $ token_t $ repo_t $ o_t),
  Term.info "import" ~doc

let id_t =
  let doc = "Bug number." in
  Arg.(required & pos 1 (some int) None & info [] ~doc)

let import_one_cmd =
  let doc = "Import single issues." in
  Term.(const import_one $ verbose_t $ token_t $ repo_t $ id_t),
  Term.info "import-one" ~doc

let default_cmd =
  let doc = "a Mantis => Github migration tool" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "mantis2github" ~version:"v0.1" ~doc ~sdocs ~exits

let cmds =
  [
    extract_cmd;
    import_cmd;
    import_one_cmd;
  ]

let () =
  Term.(exit (eval_choice default_cmd cmds))
