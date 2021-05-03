(ns y2017_d5)
(def input-instruction (util/file->int-lines "y2017_d5.txt"))
; part 1
; 현재 위치의 값만큼 이동 & 현재 위치의 값을 +1 = 1 step
; j가 count 보다 클때까지 반복
(def initial-step {:step        0
                   :offset      0
                   :instruction (vec input-instruction)})

(defn inc-instruction [offset jumps]
  (assoc jumps offset (inc (jumps offset))))

(defn jump [{:keys [step offset instruction]}]
  {:step        (inc step)
   :offset      (+ offset (instruction offset))
   :instruction (inc-instruction offset instruction)})

(defn is-in? [{:keys [_ offset instruction]}]
  (< offset (count instruction)))

; part 1 answer
(->> initial-step
     (iterate jump)
     (drop-while is-in?)
     (first)
     (:step))

(comment
  (take 1 (iterate jump initial-step))
  (first (drop-while is-in? (iterate jump initial-step)))
  (take 10 (:instruction (last (take 1 (iterate jump initial-step)))))
  (assoc (vec input-instruction) 0 (inc ((vec input-instruction) 0))))


; part 2.
; offset 값 >= 3 이면, inc 대신 dec 한다
(defn part2-instruction [offset jumps]
  (let [offset-value (jumps offset)]
    (cond (<= 3 offset-value)
          (assoc jumps offset (dec offset-value))
          :else (assoc jumps offset (inc offset-value)))))

(defn jump2 [{:keys [step offset instruction]}]
  {:step        (inc step)
   :offset      (+ offset (instruction offset))
   :instruction (part2-instruction offset instruction)})

; part 2 answer
(->> initial-step
     (iterate jump2)
     (drop-while is-in?)
     (first)
     (:step))

(comment
  (take 10 (:instruction (last (take 1 (iterate jump2 initial-step))))))