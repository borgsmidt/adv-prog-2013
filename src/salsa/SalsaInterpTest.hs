--
-- Interpolate tests
--

   interpolate 1 (0,0) (100,100)
=> [(100,100)]

   interpolate 2 (0,0) (100,100)
=> [(50,50),(100,100)]

   interpolate 5 (0,0) (100,100)
=> [(20,20),(40,40),(60,60),(80,80),(100,100)]


--
-- Testing empty input
--

   runProg 1 []
=> ([],[[]])

   runProg 10 []
=> ([],[[]])


--
-- Testing generation of key frames by using framerate 1
--

   runProg 1 [ Def (Viewdef "Default" (Const 400) (Const 400)),
               Def (Rectangle "box" (Const 10) (Const 400) (Const 20) (Const 20) Green),
               Com (Move ["box"] (Abs (Const 10) (Const 200))),
               Com (Move ["box"] (Rel (Const 100) (Const 0))),
               Com (Move ["box"] (Abs (Const 110) (Const 400))),
               Com (Move ["box"] (Rel (Minus (Const 0) (Const 100)) (Const 0))) ]
=> ([("Default",400,400)],
    [[DrawRect 10 400 20 20 "Default" "green"],
     [DrawRect 10 200 20 20 "Default" "green"],
     [DrawRect 110 200 20 20 "Default" "green"],
     [DrawRect 110 400 20 20 "Default" "green"],
     [DrawRect 10 400 20 20 "Default" "green"]])

   runProg 1  [ Def (Viewdef "One" (Const 500) (Const 500)),
                Def (Viewdef "Two" (Const 400) (Const 400)),
                Def (Group "Both" ["One","Two"]),
                Def (View "Both"),
                Def (Rectangle "larry" (Const 10) (Const 350) (Const 20)
                                   (Const 20) Blue),
                Def (Rectangle "fawn" (Const 300) (Const 350) (Const 15)
                                   (Const 25) Plum),
                Def (View "Two") ,
                Com (Par (Move ["larry"] (Abs (Const 300) (Const 350)))
                         (Move ["fawn"] (Abs (Const 10) (Const 350)))),
                Def (View "Both"),
                Com (Move ["larry","fawn"]
                              (Rel (Const 0) (Minus (Const 0) (Const 300)))) ]
=> ([("One",500,500),("Two",400,400)],
    [[DrawRect 300 350 15 25 "One" "plum",
      DrawRect 300 350 15 25 "Two" "plum",
      DrawRect 10 350 20 20 "One" "blue",
      DrawRect 10 350 20 20 "Two" "blue"],

     [DrawRect 10 350 15 25 "Two" "plum",
      DrawRect 300 350 15 25 "One" "plum",
      DrawRect 300 350 20 20 "Two" "blue",
      DrawRect 10 350 20 20 "One" "blue"],

     [DrawRect 300 50 20 20 "Two" "blue",
      DrawRect 10 50 20 20 "One" "blue",
      DrawRect 10 50 15 25 "Two" "plum",
      DrawRect 300 50 15 25 "One" "plum"]])


--
-- Testing full operation with a higher framerate that requires intermediate
-- frames (highlighted with extra indentation)
--

   runProg 3  [ Def (Viewdef "One" (Const 500) (Const 500)),
                Def (Viewdef "Two" (Const 400) (Const 400)),
                Def (Group "Both" ["One","Two"]),
                Def (View "Both"),
                Def (Rectangle "larry" (Const 10) (Const 350) (Const 20)
                                   (Const 20) Blue),
                Def (Rectangle "fawn" (Const 300) (Const 350) (Const 15)
                                   (Const 25) Plum),
                Def (View "Two") ,
                Com (Par (Move ["larry"] (Abs (Const 300) (Const 350)))
                         (Move ["fawn"] (Abs (Const 10) (Const 350)))),
                Def (View "Both"),
                Com (Move ["larry","fawn"]
                              (Rel (Const 0) (Minus (Const 0) (Const 300)))) ]
=> ([("One",500,500),("Two",400,400)],
    [[DrawRect 300 350 15 25 "One" "plum",
      DrawRect 300 350 15 25 "Two" "plum",
      DrawRect 10 350 20 20 "One" "blue",
      DrawRect 10 350 20 20 "Two" "blue"],

         [DrawRect 203 350 15 25 "Two" "plum",
          DrawRect 300 350 15 25 "One" "plum",
          DrawRect 107 350 20 20 "Two" "blue",
          DrawRect 10 350 20 20 "One" "blue"],

         [DrawRect 107 350 15 25 "Two" "plum",
          DrawRect 300 350 15 25 "One" "plum",
          DrawRect 203 350 20 20 "Two" "blue",
          DrawRect 10 350 20 20 "One" "blue"],

     [DrawRect 10 350 15 25 "Two" "plum",
      DrawRect 300 350 15 25 "One" "plum",
      DrawRect 300 350 20 20 "Two" "blue",
      DrawRect 10 350 20 20 "One" "blue"],

         [DrawRect 300 250 20 20 "Two" "blue",
          DrawRect 10 250 20 20 "One" "blue",
          DrawRect 10 250 15 25 "Two" "plum",
          DrawRect 300 250 15 25 "One" "plum"],

         [DrawRect 300 150 20 20 "Two" "blue",
          DrawRect 10 150 20 20 "One" "blue",
          DrawRect 10 150 15 25 "Two" "plum",
          DrawRect 300 150 15 25 "One" "plum"],

     [DrawRect 300 50 20 20 "Two" "blue",
      DrawRect 10 50 20 20 "One" "blue",
      DrawRect 10 50 15 25 "Two" "plum",
      DrawRect 300 50 15 25 "One" "plum"]])


























[ Def (Viewdef "Default" (Const 400) (Const 400)), Def (Rectangle "box" (Const 10) (Const 400) (Const 20) (Const 20) Green), Com (Move ["box"] (Abs (Const 10) (Const 200))), Com (Move ["box"] (Rel (Const 100) (Const 0))), Com (Move ["box"] (Abs (Const 110) (Const 400))), Com (Move ["box"] (Rel (Minus (Const 0) (Const 100)) (Const 0)))]
([("Default",400,400)],[[DrawRect 10 400 20 20 "Default" "green"],[DrawRect 10 400 20 20 "Default" "green"],[DrawRect 10 200 20 20 "Default" "green"],[DrawRect 110 200 20 20 "Default" "green"],[DrawRect 110 400 20 20 "Default" "green"]])

[ Def (Viewdef "One" (Const 500) (Const 500)) , Def (Viewdef "Two" (Const 400) (Const 400)) , Def (Group "Both" ["One","Two"]) , Def (View "Both"), Def (Rectangle "larry" (Const 10) (Const 350) (Const 20) (Const 20) Blue), Def (Rectangle "fawn" (Const 300) (Const 350) (Const 15) (Const 25) Plum), Def (View "Two") , Com (Par (Move ["larry"] (Abs (Const 300) (Const 350))) (Move ["fawn"] (Abs (Const 10) (Const 350)))) , Def (View "Both"), Com (Move ["larry","fawn"] (Rel (Const 0) (Minus (Const 0) (Const 300))))]
([("One",500,500),("Two",400,400)],
[[DrawRect 300 350 15 25 "One" "plum",DrawRect 300 350 15 25 "Two" "plum",DrawRect 10 350 20 20 "One" "blue",DrawRect 10 350 20 20 "Two" "blue"],
 [DrawRect 300 350 15 25 "Two" "plum",DrawRect 300 350 15 25 "One" "plum",DrawRect 10 350 20 20 "Two" "blue",DrawRect 10 350 20 20 "One" "blue"],
 [DrawRect 300 350 20 20 "Two" "blue",DrawRect 10 350 20 20 "One" "blue",DrawRect 10 350 15 25 "Two" "plum",DrawRect 300 350 15 25 "One" "plum"]])

[ Def (Viewdef "One" (Const 500) (Const 500)) , Def (Viewdef "Two" (Const 400) (Const 400)) , Def (Group "Both" ["One","Two"]) , Def (View "Both"), Def (Rectangle "larry" (Const 10) (Const 350) (Const 20) (Const 20) Blue), Def (Rectangle "fawn" (Const 300) (Const 350) (Const 15) (Const 25) Plum), Com (At (Par (Move ["larry"] (Abs (Const 300) (Const 350))) (Move ["fawn"] (Abs (Const 10) (Const 350)))) "Two") , Com (Move ["larry","fawn"] (Rel (Const 0) (Minus (Const 0) (Const 300))))]
([("One",500,500),("Two",400,400)],
 [[DrawRect 300 350 15 25 "One" "plum",
   DrawRect 300 350 15 25 "Two" "plum",
   DrawRect 10 350 20 20 "One" "blue",
   DrawRect 10 350 20 20 "Two" "blue"],

  [DrawRect 300 350 15 25 "Two" "plum",
   DrawRect 300 350 15 25 "One" "plum",
   DrawRect 10 350 20 20 "Two" "blue",
   DrawRect 10 350 20 20 "One" "blue"],

  [DrawRect 300 350 20 20 "Two" "blue",
   DrawRect 10 350 20 20 "One" "blue",
   DrawRect 10 350 15 25 "Two" "plum",
   DrawRect 300 350 15 25 "One" "plum"]])


([("One",500,500),("Two",400,400)],
 [[DrawRect 300 350 15 25 "One" "plum",
   DrawRect 300 350 15 25 "Two" "plum",
   DrawRect 10 350 20 20 "One" "blue",
   DrawRect 10 350 20 20 "Two" "blue"],

  [DrawRect 300 350 15 25 "Two" "plum",
   DrawRect 300 350 15 25 "One" "plum",
   DrawRect 10 350 20 20 "Two" "blue",
   DrawRect 10 350 20 20 "One" "blue"],

  [DrawRect 300 350 20 20 "Two" "blue",
   DrawRect 10 350 20 20 "One" "blue",
   DrawRect 10 350 15 25 "Two" "plum",
   DrawRect 300 350 15 25 "One" "plum"]])


([("One",500,500),("Two",400,400)],
 [[DrawRect 300 350 15 25 "One" "plum",
   DrawRect 300 350 15 25 "Two" "plum",
   DrawRect 10 350 20 20 "One" "blue",
   DrawRect 10 350 20 20 "Two" "blue"],

  [DrawRect 10 350 15 25 "Two" "plum",
   DrawRect 300 350 15 25 "One" "plum",
   DrawRect 300 350 20 20 "Two" "blue",
   DrawRect 10 350 20 20 "One" "blue"],

  [DrawRect 300 50 20 20 "Two" "blue",
   DrawRect 10 50 20 20 "One" "blue",
   DrawRect 10 50 15 25 "Two" "plum",
   DrawRect 300 50 15 25 "One" "plum"]])







([("One",500,500),("Two",400,400)],
[[DrawRect 300 350 15 25 "One" "plum",
  DrawRect 300 350 15 25 "Two" "plum",
  DrawRect 10 350 20 20 "One" "blue",
  DrawRect 10 350 20 20 "Two" "blue"],

 [DrawRect 107 350 15 25 "Two" "plum",
  DrawRect 300 350 15 25 "One" "plum",
  DrawRect 203 350 20 20 "Two" "blue",
  DrawRect 10 350 20 20 "One" "blue"],

 [DrawRect 203 350 15 25 "Two" "plum",
  DrawRect 300 350 15 25 "One" "plum",
  DrawRect 107 350 20 20 "Two" "blue",
  DrawRect 10 350 20 20 "One" "blue"],

 [DrawRect 300 350 15 25 "Two" "plum",
  DrawRect 300 350 15 25 "One" "plum",
  DrawRect 10 350 20 20 "Two" "blue",
  DrawRect 10 350 20 20 "One" "blue"],

[DrawRect 300 150 20 20 "Two" "blue",
 DrawRect 10 150 20 20 "One" "blue",
 DrawRect 10 150 15 25 "Two" "plum",
 DrawRect 300 150 15 25 "One" "plum"],

[DrawRect 300 250 20 20 "Two" "blue",
 DrawRect 10 250 20 20 "One" "blue",
 DrawRect 10 250 15 25 "Two" "plum",
 DrawRect 300 250 15 25 "One" "plum"],

[DrawRect 300 350 20 20 "Two" "blue",
 DrawRect 10 350 20 20 "One" "blue",
 DrawRect 10 350 15 25 "Two" "plum",
 DrawRect 300 350 15 25 "One" "plum"]])


[ Def (Viewdef "Default" (Const 400) (Const 400)), Def (Rectangle "box" (Const 10) (Const 400) (Const 20) (Const 20) Green), Com (Move ["box"] (Abs (Const 10) (Const 200))), Com (Move ["box"] (Rel (Const 100) (Const 0))), Com (Move ["box"] (Abs (Const 110) (Const 400))), Com (Move ["box"] (Rel (Minus (Const 0) (Const 100)) (Const 0))), Def (Rectangle "box" (Const 60) (Const 400) (Const 20) (Const 20) Blue)]
