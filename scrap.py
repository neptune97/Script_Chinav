import twint
c = twint.Config ()
c.Search = "\"vacina\""
c.Lang = "pt"
c.Since = "2020-03-01 00:00:00"
c.Store_csv = True
c.Output = "proj_chivanirus40"
twint.run.Search(c)






