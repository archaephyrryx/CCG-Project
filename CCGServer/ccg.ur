open List

val pagetitle : string = "Ultra-Rare: The Ur/Web MLP:CCG Server"

fun omit [a] (n : int) (xs : list a) : list a =
    let
	val part1 = (take n xs)

	val part2 = (drop (n+1) xs)
    in
	append part1 part2
    end

fun home () = 
    let
	val content = <xml>Welcome to the home page!</xml>
    in
	return <xml>
	  <head>
	    <title>{[pagetitle]}</title>
	  </head>
	  <body>
	    <h1>{[pagetitle]}</h1>
	    <table>
	      <tr>
		<td><a link={home ()}>Home</a></td>
		<td><a link={cards ()}>Cards</a></td>
		<td><a link={tags ()}>Tags</a></td>
		<td><a link={deck ()}>Deck</a></td>
	      </tr>
	    </table>
	    {content}
	  </body>
	</xml>
    end
    
and cards () =
    props <- source [];
    edit <- source "";
    let
	val content = 
	    let
		val dynamic : xbody = <xml><table>
		  <dyn signal={ls <- signal props; return (mapXi (fn (i : int) (s : string) => (<xml><tr><td>{[s]}</td><td><button value="-" onclick={fn _ => set props (omit i ls)}/></td></tr></xml>)) ls)}/>
																									   </table></xml>
	        val static : xbody = <xml>
		  <ctextbox source={edit} placeholder="New Tag"/>
		  <button value="+" onclick={fn _ => entry <- get edit; ls <- get props; set props (append ls (entry :: [])); set edit ("")}/>
		  </xml>
	    in
		<xml>{dynamic}{static}</xml>
	    end
    in
	return <xml>
	  <head>
	    <title>{[pagetitle]}</title>
	  </head>
	  <body>
	    <h1>{[pagetitle]}</h1>
	    <table>
	      <tr>
	      <td><a link={home ()}>Home</a></td>
	      <td><a link={cards ()}>Cards</a></td>
	      <td><a link={tags ()}>Tags</a></td>
	      <td><a link={deck ()}>Deck</a></td>
	    </tr>
	</table>
	{content}
	  </body>
	</xml>
    end

and tags () =
    let
	val content = <xml>This part has not been developed yet. Stay tuned!</xml>
    in
	return <xml>
	  <head>
	    <title>{[pagetitle]}</title>
	  </head>
	  <body>
	    <h1>{[pagetitle]}</h1>
	    <table>
	      <tr>
		<td><a link={home ()}>Home</a></td>
		<td><a link={cards ()}>Cards</a></td>
		<td><a link={tags ()}>Tags</a></td>
		<td><a link={deck ()}>Deck</a></td>
	      </tr>
	    </table>
	    {content}
	  </body>
	</xml>
    end
    
and deck () =
    let
	val content = <xml>This part has not been developed yet. Stay tuned!</xml>
    in
	return <xml>
	  <head>
	    <title>{[pagetitle]}</title>
	  </head>
	  <body>
	    <h1>{[pagetitle]}</h1>
	    <table>
	      <tr>
		<td><a link={home ()}>Home</a></td>
		<td><a link={cards ()}>Cards</a></td>
		<td><a link={tags ()}>Tags</a></td>
		<td><a link={deck ()}>Deck</a></td>
	      </tr>
	    </table>
	    {content}
	  </body>
	</xml>
    end

    

fun main () =
    let
	val content = <xml/>
    in
	return <xml>
	  <head>
	    <title>{[pagetitle]}</title>
	  </head>
	  <body>
	    <h1>{[pagetitle]}</h1>
	    <table>
	      <tr>
		<td><a link={home ()}>Home</a></td>
		<td><a link={cards ()}>Cards</a></td>
		<td><a link={tags ()}>Tags</a></td>
		<td><a link={deck ()}>Deck</a></td>
	      </tr>
	    </table>
	    {content}
	  </body>
	</xml>
    end
