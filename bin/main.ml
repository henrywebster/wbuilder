type post = {
  blog_name : string;
  title : string;
  content : string;
  created_at : string;
  slug : string;
}

type blog = { name : string; posts : post list }

(*
   TODO
   [x] Load full content into DB and render
   [x] Generate pages
   [ ] Media management
   [x] Add unique constraint to slug
   [x] Have different css file
   [x] Have title on each page
   [ ] Run with GitHub actions
   [ ] Set up Terraform - s3 bucket
   [x] Narrow down all the font loading
   [ ] Only get post names for blog?
   [x] Formatting for HTML and CSS
*)

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

let write_post post =
  Core.Out_channel.write_all
    (Printf.sprintf "site/post/%s.html" post.slug)
    ~data:
      (Mustache.render
         (Mustache.of_string (Core.In_channel.read_all "post.mustache"))
         (convert_post post))

let () =
  let output_file = Sys.getenv "WBUILDER_OUTPUT_FILE" in
  let template_file = Sys.getenv "WBUILDER_TEMPLATE_FILE" in

  let convert_blog blog =
    `O
      [
        ("name", `String blog.name);
        ("posts", `A (List.map convert_post blog.posts));
      ]
  in
  let from_db =
    let prepare_conn =
      Sqlite3.prepare
        (Sqlite3.db_open (Sys.getenv "WBUILDER_DB_FILE") ~mode:`READONLY)
    in
    let info_statement = prepare_conn "SELECT name FROM hello" in
    ignore (Sqlite3.step info_statement);
    let name =
      Sqlite3.Data.to_string_exn (Array.get (Sqlite3.row_data info_statement) 0)
    in
    let posts =
      let to_array acc post =
        let get = Array.get post in
        acc
        @ [
            {
              blog_name = Sqlite3.Data.to_string_exn (get 0);
              title = Sqlite3.Data.to_string_exn (get 1);
              content = Sqlite3.Data.to_string_exn (get 2);
              created_at = Sqlite3.Data.to_string_exn (get 3);
              slug = Sqlite3.Data.to_string_exn (get 4);
            };
          ]
      in

      match
        Sqlite3.fold
          (prepare_conn
             "SELECT name, title, content, strftime('%Y-%m-%d', \
              DATE(created_at, 'unixepoch')) AS created_at, slug FROM post \
              JOIN (SELECT name FROM hello ORDER BY rowid ASC LIMIT 1) ORDER \
              BY created_at DESC")
          ~f:to_array ~init:[]
      with
      | _, value -> value
    in
    { name; posts }
  in
  let blog = from_db in
  ignore
    (Core.Out_channel.write_all output_file
       ~data:
         (Mustache.render
            (Mustache.of_string (Core.In_channel.read_all template_file))
            (convert_blog blog)));

  ignore (List.map write_post blog.posts)
