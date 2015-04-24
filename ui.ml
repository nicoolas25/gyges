let _ = GtkMain.Main.init ()

let quit_game _ =
  print_endline "Exiting Gyges..." ;
  flush stdout ;
  false

class cell (width,height) ~text ~packing =
  let _box = GBin.event_box ~packing ~width ~height () in
  let _label = GMisc.label ~text ~packing:_box#add () in

  object (self)
    val box = _box
    val label = _label
    val mutable on_click = None

    initializer box#event#add [`BUTTON_RELEASE]

    method del_on_click =
      match on_click with
        | None -> ()
        | Some signal -> GtkSignal.disconnect box#as_widget signal

    method set_on_click callback =
      self#del_on_click ;
      ignore (box#event#connect#button_release ~callback)
  end

let () =
  let window = GWindow.window ~title:"Gyges"  () in

  ignore (window#event#connect#delete ~callback:quit_game) ;
  ignore (window#connect#destroy ~callback:GMain.Main.quit) ;

  let layout = GPack.vbox ~packing:window#add ~border_width:5 () in
  let top_cell = new cell (300,50) ~text:"_" ~packing:layout#add in
  let grid = GPack.table ~packing:layout#add ~width:300 ~height:300 ~rows:6 ~columns:6 () in
  let bot_cell = new cell (300,50) ~text:"_" ~packing:layout#add in
  let status = GMisc.label ~text:"Welcome to Gyges!" ~packing:layout#add () in

  for i = 0 to 5 do
    for j = 0 to 5 do
      let cell = new cell (50,50) ~text:"_" ~packing:(grid#attach ~left:j ~top:i) in
      cell#set_on_click (fun _ ->
        let position = Printf.sprintf "%i, %i" i j in
        status#set_text position ;
        flush stdout ;
        true)
    done
  done;

  window#show () ;
  GMain.Main.main ()
