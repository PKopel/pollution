
(defmodule pollution_sup
  (behaviour supervisor)
  (export (init 1)
          (start_link 0)))

(defun start_link () (supervisor:start_link #('local (MODULE)) (MODULE) '[]))

(defun init ('[])
  (let* ((sup_flags #M('strategy 'one_for_all 'intensity 0 'period 1))
         (child_spec #M('id pollution_lfe_gen_server
                            'start `#(pollution_gen_server start_link [])
                            'restart 'transient
                            'shutdown 'brutal_kill
                            'type 'worker
                            'modules (pollution_gen_server pollution))))
                    #('ok #(sup_flags child_spec))))
;;internal functions