#!/usr/bin/sbcl --script

(load "../src/load")
(load "../utils/test")

(defun test-utils ()

  (do-test (vec:norm (vec:vec 3.0d0 0.0d0))
           (vec:vec 1.0d0 0.0d0))

  (do-test (vec:sub (vec:vec 1.0d0 2.0d0) (vec:vec 2.0d0 3.0d0))
           (vec:vec -1.d0 -1.d0))

  (do-test (vec:add (vec:vec 1.0d0 2.0d0) (vec:vec 2.0d0 3.0d0))
           (vec:vec 3.0d0 5.0d0))

  (do-test (vec:nsub (vec:vec 1.0d0 2.0d0) (vec:vec 2.0d0 10.0d0))
           (vec:vec -0.12403473458920847d0 -0.9922778767136677d0))

  (do-test (vec:len2 (vec:vec 1.0d0 2.0d0))
           5)

  (do-test (vec:len (vec:vec 1.0d0 2.0d0))
           2.23606797749979d0)

  (do-test (vec:len (vec:vec 1.0d0 2.0d0))
           2.23606797749979d0)

  (do-test (vec:dst (vec:vec 1.0d0 2.0d0) (vec:vec 1.0d0 3.0d0))
           1.0d0)

  (do-test (vec:mid (vec:vec 1.0d0 2.0d0) (vec:vec 3.0d0 4.0d0))
           (vec:vec 2.0d0 3.0d0))

  (do-test (vec:sum (list (vec:vec 1.0d0 2.0d0) (vec:vec 0.5d0 4.322d0)))
           (vec:vec 1.5d0 6.322d0))

  (do-test (vec:on-line 0.34d0 (vec:vec 33d0 88d0) (vec:vec 32d0 733d0))
           (vec:vec 32.66d0 307.3d0))

  (do-test (vec:on-circ 0.34d0 388d0 :xy (vec:vec 32d0 733d0))
           (vec:vec -175.9007964518508d0 1060.5992350947818d0))

  (do-test (to-list
             (math:path-tangents (list (vec:vec 1.0d0 2.0d0)
                                       (vec:vec 1.0d0 2.0d0)
                                       (vec:vec 0.5d0 4.322d0))))
           (list (vec:vec 0d0)
                 (vec:vec -0.21050655592417808d0 0.977592445711883d0)))

  (do-test (math:inc 0.1 0.4)
           0.5)

  (do-test (math:inc 0.1 -0.4)
           0.7)

  (do-test (math:linspace 1 0d0 10d0)
           (list 0.0))

  (do-test (math:linspace 3 0d0 10d0)
           (list 0.0 5.0 10.0))

  (do-test (math:linspace 2 0d0 10d0 :end nil)
           (list 0.0 5.0))

  (do-test (math:linspace 2 0d0 10d0 :end t)
           (list 0.0 10.0))

  (do-test (math:range 2 5)
           (list 2 3 4))

  (do-test (math:range 5)
           (list 0 1 2 3 4))

  (do-test
    (let ((a (list)))
      (math:with-linspace (10 0d0 7d0 v)
        (setf a (append a (list v))))
      a)
   '(0.0d0 0.7777777777777778d0 1.5555555555555556d0 2.3333333333333335d0
     3.111111111111111d0 3.888888888888889d0 4.666666666666667d0
     5.444444444444445d0 6.222222222222222d0 7.0d0))

  (do-test
    (let ((a (list)))
      (math:with-linspace (10 0d0 7d0 v :end nil)
        (setf a (append a (list v))))
      a)
   '(0.0d0 0.7d0 1.4d0 2.0999999999999996d0 2.8d0 3.5d0 4.199999999999999d0
     4.8999999999999995d0 5.6d0 6.3d0))

  (do-test
    (vec:segdst (list (vec:vec 0d0 0d0) (vec:vec 100d0 0d0)) (vec:vec 0d0 200d0))
    200d0)

  (do-test
    (vec:segdst (list (vec:vec 0d0 0d0) (vec:vec 100d0 3d0)) (vec:vec 41d0 202d0))
    200.67971443818558d0)

  (do-test
    (vec:segx (list (vec:vec 0d0 0d0) (vec:vec 100d0 0d0))
              (list (vec:vec 0d0 1d0) (vec:vec 100d0 1d0)) :parallel :par)
    :par)

  (do-test
    (vec:segx (list (vec:vec 0d0 0d0) (vec:vec 1d0 1d0))
              (list (vec:vec 0d0 1d0) (vec:vec 1d0 0d0)) :parallel :par)
    t)

  (do-test
    (vec:cross (vec:vec 1d0 2d0) (vec:vec 3d0 -7.1d0))
    -13.1d0))


(defun test-rnd ()

  (rnd:set-rnd-state 1)

  (do-test
    (length (rnd:rndspace 10 0d0 10d0))
    10)

  (do-test
    (rnd:rndspace 10 0d0 10d0) '(8.383887417540674d0 3.704390759927394d0
                                 4.089044985321939d0 7.5623438794824605d0
                                 0.5477479401961061d0 3.409356250400757d0
                                 8.3460946770173d0 1.1737959928376207d0
                                 2.8077405846385473d0 3.962028297321658d0))

  (do-test
    (rnd:rndspace 10 0d0 10d0 :order t)
     '(0.7810904793737161d0 1.700886055024764d0 3.396607010299655d0
       3.8464500251059364d0 6.014897498803242d0 6.268483093269445d0
       7.527788782312825d0 7.562853532104885d0 7.892139712781054d0
       9.365232493948968d0))

  (do-test (rnd:rndspacei 10 0 10)
           '(7 3 6 5 1 9 3 4 8 1))

  (do-test (rnd:rndspacei 10 0 10 :order t)
           '(0 0 2 2 3 4 4 7 9 9))

  (do-test (length (rnd:nrndi 9 4))
           9)

  (do-test (length (rnd:nrnd 11 4d0))
           11)

  (do-test (length (rnd:nrnd 12 4d0))
           12)

  (do-test (length (rnd:nrnd* 12 4d0))
           12)

  (do-test (rnd:bernoulli 4 0.5d0)
           '(0.0d0 0.0d0 0.0d0 0.0d0))

  (do-test
    (let ((a (list)))
      (rnd:with-rndspace (10 0d0 7d0 v)
        (setf a (append a (list v))))
      a)
   '(6.507759172138558d0 2.2881798617968423d0 1.7037377515259524d0
     1.6064438902476477d0 6.2928480610573665d0 6.298771369144957d0
     1.097454636438975d0 5.713911684630423d0 3.318378773155661d0
     5.647317315258949d0))

  (do-test
    (let ((a (list)))
      (rnd:with-on-line (10 (vec:vec 1d0 1d0) (vec:vec 4d0 3d0) v)
        (setf a (append a (list v))))
      a)
   (list (vec:vec 1.4420775452437415d0 1.2947183634958277d0)
         (vec:vec 1.0172227079557705d0 1.011481805303847d0)
         (vec:vec 1.702867945470302d0 1.4685786303135346d0)
         (vec:vec 2.9598710402505835d0 2.306580693500389d0)
         (vec:vec 1.1623459120025506d0 1.1082306080017004d0)
         (vec:vec 3.3156539222026513d0 2.543769281468434d0)
         (vec:vec 3.143617069170394d0 2.429078046113596d0)
         (vec:vec 3.4512122094895474d0 2.6341414729930315d0)
         (vec:vec 1.7401594182758866d0 1.4934396121839244d0)
         (vec:vec 1.0949266500503054d0 1.0632844333668703d0)))

  (do-test
    (let ((a (list)))
      (rnd:with-in-circ (10 4d0 v)
        (setf a (append a (list v))))
      a)
    (list (vec:vec -3.2289266618005463d0 1.173326893900633d0)
          (vec:vec -2.267965042218162d0 3.268859019509251d0)
          (vec:vec -1.4143813501406504d0 3.1585446412766736d0)
          (vec:vec -0.8043636451397664d0 1.3535842643178702d0)
          (vec:vec 2.0421463502216497d0 -0.07975392718826195d0)
          (vec:vec -3.384193548911458d0 -6.350251329993679d-4)
          (vec:vec 0.5328441305158991d0 0.8393134784537689d0)
          (vec:vec 0.20238689972914906d0 -0.101591706130127d0)
          (vec:vec -1.162668476292421d0 -2.635125988346128d0)
          (vec:vec -0.039436399757947375d0 -0.42073258274167163d0)))

  (do-test
    (rnd:on-line (vec:vec 101d0 204d0) (vec:vec 433d0 454d0))
    (vec:vec 328.1083568590696d0 375.01532896014277d0))

  (do-test
    (rnd:on-circ 303d0 :xy (vec:vec 303d0 73d0))
    (vec:vec 306.14778707543655d0 375.98364879400293d0))

  (do-test
    (rnd:in-circ 303d0 :xy (vec:vec 303d0 73d0))
    (vec:vec 243.51609955461907d0 231.38334433095946d0))

  (do-test
    (rnd:non-line 5 (vec:vec 101d0 204d0) (vec:vec 433d0 454d0))
    (list (vec:vec 412.8114993587596d0 438.7978157821985d0)
          (vec:vec 141.5239773978766d0 234.51504322129261d0)
          (vec:vec 113.3819941657358d0 213.32379078745166d0)
          (vec:vec 300.62005634338436d0 354.3163074874882d0)
          (vec:vec 255.69499795342622d0 320.487197254086d0)))

  (do-test
    (rnd:nin-circ 5 20d0 :xy (vec:vec 433d0 454d0))
    (list (vec:vec 437.0273521546584d0 439.0174212934645d0)
          (vec:vec 426.7886779194167d0 440.85162023013874d0)
          (vec:vec 428.35235989996613d0 456.86207789540106d0)
          (vec:vec 428.149958120093d0 454.189520107701d0)
          (vec:vec 429.3064265983025d0 456.20125820264457d0))))


(defun test-bzspl ()
(let ((pts-a (list (vec:vec -20.0d0 99.0d0)
                   (vec:vec 0.0d0 1.0d0)
                   (vec:vec 10.0d0 20.0d0)
                   (vec:vec 100.0d0 100.0d0)))
      (pts-b (list (vec:vec -20.0d0 99.0d0)
                   (vec:vec 0.0d0 1.0d0)
                   (vec:vec 10.0d0 20.0d0)
                   (vec:vec 100.0d0 100.0d0)
                   (vec:vec -3.0d0 -17.0d0)
                   (vec:vec 0.0d0 4.0d0)))
      (pts-c (list (vec:vec -32.0d0 79.0d0)
                   (vec:vec 0.3d0 3.0d0)
                   (vec:vec 10.1d0 25.0d0))))
    (do-test
      (bzspl:pos* (bzspl:make pts-c) (math:linspace 5 0d0 1d0))
      (list (vec:vec -32.0d0 79.0d0)
            (vec:vec -17.256249999999998d0 47.125d0)
            (vec:vec -5.324999999999999d0 27.5d0)
            (vec:vec 3.7937499999999993d0 20.125d0)
            (vec:vec 10.1d0 25.0d0)))

    (do-test
      (bzspl:pos* (bzspl:make pts-c :closed t) (math:linspace 5 0d0 1d0))
      (list (vec:vec -15.85d0 41.0d0)
            (vec:vec 2.046875d0 11.5625d0)
            (vec:vec 3.6124999999999985d0 29.000000000000004d0)
            (vec:vec -19.150000000000002d0 61.4375d0)
            (vec:vec -15.85d0 41.0d0)))

    (do-test
      (bzspl:pos* (bzspl:make pts-a) (math:linspace 10 0d0 1d0))
      (list (vec:vec -20.0d0 99.0d0)
            (vec:vec -11.851851851851853d0 60.75308641975309d0)
            (vec:vec -5.185185185185186d0 33.12345679012346d0)
            (vec:vec -8.881784197001252d-16 16.111111111111114d0)
            (vec:vec 3.7037037037037024d0 9.716049382716054d0)
            (vec:vec 7.160493827160495d0 13.481481481481485d0)
            (vec:vec 17.777777777777775d0 24.666666666666664d0)
            (vec:vec 36.7901234567901d0 42.814814814814795d0)
            (vec:vec 64.19753086419752d0 67.92592592592591d0)
            (vec:vec 100.0d0 100.0d0)))

    (do-test
      (bzspl:pos* (bzspl:make pts-b) (math:linspace 10 0d0 1d0))
      (list (vec:vec -20.0d0 99.0d0)
            (vec:vec -5.185185185185186d0 33.12345679012346d0)
            (vec:vec 3.7037037037037024d0 9.716049382716054d0)
            (vec:vec 12.777777777777775d0 20.22222222222222d0)
            (vec:vec 36.9753086419753d0 43.728395061728385d0)
            (vec:vec 70.23456790123457d0 72.91358024691358d0)
            (vec:vec 72.11111111111111d0 69.55555555555556d0)
            (vec:vec 37.728395061728435d0 29.481481481481524d0)
            (vec:vec 8.098765432098773d0 1.0370370370370405d0)
            (vec:vec 0.0d0 4.0d0)))

    (do-test
      (bzspl:pos* (bzspl:make pts-a :closed t) (math:linspace 10 0d0 1d0))
      (list (vec:vec -10.0d0 50.0d0)
            (vec:vec -2.098765432098766d0 18.000000000000004d0)
            (vec:vec 3.8271604938271597d0 9.111111111111114d0)
            (vec:vec 12.777777777777775d0 20.22222222222222d0)
            (vec:vec 36.9753086419753d0 43.728395061728385d0)
            (vec:vec 69.81481481481482d0 75.77777777777779d0)
            (vec:vec 68.33333333333334d0 95.33333333333331d0)
            (vec:vec 27.53086419753091d0 98.79012345679014d0)
            (vec:vec -5.061728395061721d0 83.97530864197533d0)
            (vec:vec -10.0d0 50.0d0)))

    (do-test
      (bzspl:pos* (bzspl:make pts-b :closed t) (math:linspace 10 0d0 1d0))
      (list (vec:vec -10.0d0 50.0d0)
            (vec:vec 1.1111111111111107d0 10.666666666666668d0)
            (vec:vec 12.777777777777779d0 20.22222222222222d0)
            (vec:vec 55.0d0 60.0d0)
            (vec:vec 72.11111111111111d0 69.55555555555556d0)
            (vec:vec 20.055555555555546d0 10.166666666666655d0)
            (vec:vec -1.5d0 -6.5d0)
            (vec:vec -4.611111111111106d0 23.944444444444418d0)
            (vec:vec -14.444444444444446d0 72.44444444444444d0)
            (vec:vec -10.0d0 50.0d0)))

    (rnd:set-rnd-state 1)

    (do-test
      (let ((a (list)))
        (bzspl:with-rndpos ((bzspl:make pts-b :closed t) 5 v)
          (setf a (append a (list v))))
        a)
      (list (vec:vec -10.084970357192768d0 51.903581889280694d0)
            (vec:vec 72.94161639142136d0 70.67347981875085d0)
            (vec:vec -9.972643319179285d0 49.866015920500494d0)
            (vec:vec 4.718551216740959d0 -4.763338116541952d0)
            (vec:vec 35.77978017789252d0 42.62675016658848d0)))

    (do-test (length (bzspl:adaptive-pos (bzspl:make pts-a)))
             226)

    (do-test
      (bzspl:adaptive-pos (bzspl:make (list (vec:vec 0d0 0d0)
                                            (vec:vec 1d0 2d0)
                                            (vec:vec -3d0 5d0))))
      (list (vec:vec 0.0d0 0.0d0)
            (vec:vec 0.19444444444444448d0 0.6944444444444444d0)
            (vec:vec 0.1111111111111111d0 1.4444444444444444d0)
            (vec:vec -0.25d0 2.25d0)
            (vec:vec -0.8888888888888888d0 3.111111111111111d0)
            (vec:vec -1.8055555555555551d0 4.027777777777777d0)
            (vec:vec -3.0d0 5.0d0)))

    (do-test (bzspl:len (bzspl:make pts-a))
             225.14997459916174d0)

    (do-test (bzspl:len (bzspl:make pts-a :closed t))
             275.1377195594309d0)))


(defun test-hset ()

  (let ((hs (hset:make)))

    (do-test (hset:add hs 1)
             t)

    (do-test (hset:add hs 1)
             nil)

    (do-test (hset:add hs 20)
             t)

    (do-test (hset:add hs 40)
             t)

    (do-test (hset:add hs 73)
             t)

    (do-test (hset:num hs)
             4)

    (do-test (hset:del hs 1)
             t)

    (do-test (hset:del hs 1)
             nil)

    (do-test (hset:mem hs 40)
             t)

    (do-test (hset:mem* hs (list 40 88))
             (list t nil))

    (do-test (sort (hset:to-list hs) #'<)
             (list 20 40 73)))


  (let ((hs (hset:make :init (list 1 2 3))))

    (do-test (hset:to-list hs)
             (list 1 2 3))))


(defun test-graph ()

  (let ((grph (graph:make)))

    (do-test (graph:add grph 1 1)
             t)

    (do-test (graph:add grph 1 2)
             t)

    (do-test (graph:add grph 1 2)
             nil)

    (do-test (graph:add grph 2 1)
             nil)

    (do-test (graph:get-num-edges grph)
             4)

    (do-test (graph:get-edges grph)
             '#((1 1) (1 2)))

    (do-test (graph:add grph 20 5)
             t)

    (do-test (graph:get-edges grph)
             '#((1 1) (1 2) (5 20)))

    (do-test (graph:del grph 1 2)
             t)

    (do-test (graph:del grph 1 2)
             nil)

    (do-test (graph:get-edges grph)
             '#((1 1) (5 20)))

    (do-test (graph:get-num-edges grph)
             4)

    (do-test (graph:mem grph 1 4)
             nil)

    (do-test (graph:mem grph 1 1)
             t)

    (do-test (sort (graph:get-verts grph) #'<)
             '(1 5 20))

    (do-test (graph:del grph 1 1)
             t)

    (do-test (graph:get-edges grph)
             '#((5 20)))

    (do-test (sort (graph:get-verts grph) #'<)
             '(5 20))

    (do-test (graph:del grph 5 20)
             t)

    (do-test (sort (graph:get-verts grph) #'<)
             nil)))


(defun test-linear-path ()

  (rnd:set-rnd-state 1)

  (let ((apath (lin-path:make (rnd:nin-box 33 300d0 300d0))))
    (do-test
      (lin-path:pos* apath (rnd:rndspace 20 0d0 1d0))
      (list (vec:vec -9.438347648872167d0 -175.225149152222d0)
            (vec:vec 116.89881659438566d0 -26.809245714782996d0)
            (vec:vec -65.36161703283702d0 185.74828332183736d0)
            (vec:vec -105.48322687473d0 -9.890268437414548d0)
            (vec:vec 267.68539603378895d0 81.3043899209714d0)
            (vec:vec -132.40421434059442d0 195.15630286314308d0)
            (vec:vec -97.44930816032483d0 177.55858394044853d0)
            (vec:vec 266.5652538517507d0 -0.12485684153419593d0)
            (vec:vec 164.1995002008751d0 105.86496577929238d0)
            (vec:vec -26.728304107996706d0 -204.3541153362095d0)
            (vec:vec -177.47950464695646d0 150.76511064797404d0)
            (vec:vec 14.766557204046116d0 -93.04887526533449d0)
            (vec:vec 91.93635520751945d0 -108.1690462450528d0)
            (vec:vec 170.5374458058014d0 143.51512360627012d0)
            (vec:vec -104.24865074429103d0 64.19229247140761d0)
            (vec:vec 13.85574261994833d0 168.04649139453574d0)
            (vec:vec 90.01632000058807d0 -106.09734739384757d0)
            (vec:vec 33.09742525543997d0 -109.45542035126164d0)
            (vec:vec -190.2979189072196d0 196.3589224835634d0)
            (vec:vec 125.93727427384897d0 112.82256366840846d0)))

    (do-test
      (lin-path:pos* apath (rnd:rndspace 5 0d0 1d0))
      (list (vec:vec -187.5705364827427d0 158.41359771733312d0)
            (vec:vec 187.35293276640311d0 -29.852529549097056d0)
            (vec:vec -5.522855469879147d0 -168.62858891534736d0)
            (vec:vec 55.13044208493619d0 156.21422990619857d0)
            (vec:vec 200.2368412092638d0 114.61642591669907d0))))

  (let ((apath (lin-path:make (rnd:nin-box 12 300d0 300d0) :closed t)))
    (do-test
      (lin-path:pos* apath (rnd:rndspace 20 0d0 1d0))
      (list (vec:vec 27.920854552669738d0 72.74990940914415d0)
            (vec:vec 20.347116016838214d0 60.03424910291827d0)
            (vec:vec 90.54515305308234d0 56.63138904522498d0)
            (vec:vec -25.3222203310782d0 -183.1677471273439d0)
            (vec:vec 211.41460164089594d0 295.28993948962716d0)
            (vec:vec -55.15685103609334d0 -219.1553983499264d0)
            (vec:vec 3.5116120355960163d0 -212.55092858244558d0)
            (vec:vec 22.934457423851427d0 -210.36444561614667d0)
            (vec:vec -16.230443414179007d0 -1.3763499500023784d0)
            (vec:vec -114.29745056967332d0 -38.66755954976239d0)
            (vec:vec 120.46277829407285d0 -199.3854150955505d0)
            (vec:vec 161.48480036064814d0 -188.4310964342001d0)
            (vec:vec -250.3371679969546d0 59.6286189885981d0)
            (vec:vec 211.5358543713892d0 296.19316067596804d0)
            (vec:vec 141.119808254249d0 262.8012870055113d0)
            (vec:vec 76.43639032666181d0 -34.81227407014987d0)
            (vec:vec -83.50672601072395d0 -3.0479216467079766d0)
            (vec:vec 84.97443810482987d0 -186.13693462985242d0)
            (vec:vec -66.5778671576494d0 -220.44109345321482d0)
            (vec:vec -3.2600089472467175d0 192.22456394293548d0)))

    (do-test
      (lin-path:pos* apath (rnd:rndspace 5 0d0 1d0))
      (list (vec:vec -79.39852989264577d0 -107.43017344211452d0)
            (vec:vec -55.656453545073305d0 162.25392644684086d0)
            (vec:vec 96.08643196039844d0 -190.68148658414955d0)
            (vec:vec -87.73518977972896d0 -57.86025399494548d0)
            (vec:vec -11.885662338531773d0 5.918166490124946d0)))

    (do-test
      (lin-path:rndpos apath 5)
       (list (vec:vec 129.62232574809065d0 -187.47570071925472d0)
             (vec:vec 196.2965520484057d0 182.6743915136004d0)
             (vec:vec 95.34514262347875d0 -186.44790001400926d0)
             (vec:vec 201.75690539451068d0 272.4593392134565d0)
             (vec:vec 173.48377006092858d0 -193.41669665923908d0)))))


(defun main ()
  (test-title (test-utils))
  (test-title (test-rnd))
  (test-title (test-bzspl))
  (test-title (test-hset))
  (test-title (test-graph))
  (test-title (test-linear-path))
  (test-title (test-summary)))

(main)

