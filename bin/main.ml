type post = {
  blog_name : string;
  title : string;
  content : string;
  created_at : string;
  slug : string;
}

type image = { filename : string; data : string }
type blog = { name : string; posts : post list; images : image list }

(*
   TODO
   [x] Load full content into DB and render
   [x] Generate pages
   [x] Media management
   [x] Add unique constraint to slug
   [x] Have different css file
   [x] Have title on each page
   [x] Run with GitHub actions
   [ ] Set up Terraform - s3 bucket
   [x] Narrow down all the font loading
   [ ] Only get post names for blog?
   [x] Formatting for HTML and CSS
   [ ] Write pictures one at a time
   [ ] Program manages site folder
   [ ] Only write pictures used in posts
   [ ] All-or-nothing (delete site folder if another part fails)
   [ ] Concurrent writing of files
   [x] Fix hardcoded files
   [ ] Alt tags on images
   [x] Fix database names
   [ ] CI Checks 
*)

let () =
  let base_folder = Sys.getenv "WBUILDER_BASE" in
  let template_file = Sys.getenv "WBUILDER_HOME_TEMPLATE_FILE" in
  let convert_post post =
    match post with
    | { blog_name; title; content; created_at; slug } ->
        `O
          [
            ("blog_name", `String blog_name);
            ("title", `String title);
            ("content", `String content);
            ("created_at", `String created_at);
            ("slug", `String slug);
          ]
  in
  let convert_blog blog =
    `O
      [
        ("name", `String blog.name);
        ("posts", `A (List.map convert_post blog.posts));
      ]
  in
  let write_post post =
    ignore
      (Core.Out_channel.write_all
         (Printf.sprintf "%s/%s/%s.html" base_folder
            (Sys.getenv "WBUILDER_POST_FOLDER")
            post.slug)
         ~data:
           (Mustache.render
              (Mustache.of_string
                 (Core.In_channel.read_all
                    (Sys.getenv "WBUILDER_POST_TEMPLATE_FILE")))
              (convert_post post)))
  in
  let from_db =
    let prepare_conn =
      Sqlite3.prepare
        (Sqlite3.db_open (Sys.getenv "WBUILDER_DB_FILE") ~mode:`READONLY)
    in
    let db_to_list converter accumulator row = accumulator @ converter row in
    let query_db_rows query converter =
      match
        Sqlite3.fold (prepare_conn query) ~f:(db_to_list converter) ~init:[]
      with
      | _, value -> value
    in

    let images =
      let converter row =
        [
          {
            filename = Sqlite3.Data.to_string_exn ((Array.get row) 0);
            data = Sqlite3.Data.to_string_exn ((Array.get row) 1);
          };
        ]
      in
      query_db_rows "SELECT name, data FROM image" converter
    in

    let posts =
      let converter row =
        [
          {
            blog_name = Sqlite3.Data.to_string_exn ((Array.get row) 0);
            title = Sqlite3.Data.to_string_exn ((Array.get row) 1);
            content = Sqlite3.Data.to_string_exn ((Array.get row) 2);
            created_at = Sqlite3.Data.to_string_exn ((Array.get row) 3);
            slug = Sqlite3.Data.to_string_exn ((Array.get row) 4);
          };
        ]
      in
      query_db_rows
        "SELECT name, title, content, strftime('%Y-%m-%d', DATE(created_at, \
         'unixepoch')) AS created_at, slug FROM post JOIN (SELECT name FROM \
         blog ORDER BY rowid ASC LIMIT 1) ORDER BY created_at DESC"
        converter
    in

    let name =
      let info_statement = prepare_conn "SELECT name FROM blog" in
      ignore (Sqlite3.step info_statement);
      Sqlite3.Data.to_string_exn (Array.get (Sqlite3.row_data info_statement) 0)
    in

    { name; posts; images }
  in

  let blog = from_db in
  ignore
    (Core.Out_channel.write_all
       (Printf.sprintf "%s/index.html" base_folder)
       ~data:
         (Mustache.render
            (Mustache.of_string (Core.In_channel.read_all template_file))
            (convert_blog blog)));

  ignore (List.map write_post blog.posts);
  let write_image image =
    match image with
    | { filename; data } ->
        Core.Out_channel.write_all
          (Printf.sprintf "%s/%s/%s" base_folder
             (Sys.getenv "WBUILDER_IMG_FOLDER")
             filename)
          ~data
  in
  ignore (List.iter write_image blog.images)
