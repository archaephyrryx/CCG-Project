

fun target () = return <xml><body>
  Welcome!
</body></xml>

fun main () = return <xml>
  <head>
    <title>Hello world!</title>
  </head>

  <body>
    <h1>Hello world!</h1>
    <a link={target ()}>Go here!</a>
  </body>
</xml>
