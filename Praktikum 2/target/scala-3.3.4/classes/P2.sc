import scala.io.Source

case class Track(title: String, length: String, rating:
Int, features: List[String], writers: List[String])
case class Album(title: String, date: String, artist:
String, tracks: List[Track])

def fileContent() : List[Char] = Source.fromResource("alben.xml").toList

def createToken(input :List[Char], token:String) : String = input match
  case Nil => ""
  case c::rs => c match
    case '<' | '>' => token
    case '\n' | '\r' | '\t' => createToken(rs, token)
    case x => createToken(rs, token + x)

def createTokenListHelper(Cs :List[Char]):List[String] = Cs match
  case Nil => Nil
  case c::rs => c match
    case '<' |'>' => createToken(rs, "") match
      case "" => createTokenListHelper(rs)
      case _ => createToken(rs, "")::createTokenListHelper(rs)
    case _ => createTokenListHelper(rs)

def createTokenList(Cs :List[Char]) : List[String] = createTokenListHelper(Cs)

createTokenList(fileContent())

def parseTrack(source: List[String], TrackList: List[Track], currentTrack: Track): List[Track] = source match {
  case "feature"::x::rs => parseTrack(rs, TrackList, currentTrack.copy(features = currentTrack.features :+ x))
  case "writing"::x::rs => parseTrack(rs, TrackList, currentTrack.copy(writers =  currentTrack.writers :+ x))
  case "title"::x::rs => parseTrack(rs, TrackList, currentTrack.copy(title = x))
  case "length"::x::rs => parseTrack(rs, TrackList, currentTrack.copy(length = x))
  case "rating"::x::rs => parseTrack(rs, TrackList, currentTrack.copy(rating = x.toInt))
  case "/track"::rs => TrackList :+ currentTrack
  case _::rs => parseTrack(rs, TrackList, currentTrack)
  case Nil => Nil
}

def parseAlbum(source: List[String], currentAlb: Album):Album = source match {
  case "track"::rs => parseAlbum(rs, currentAlb.copy(tracks=parseTrack(rs, currentAlb.tracks, Track("","",0,Nil,Nil))))
  case "title"::x::rs => parseAlbum(rs, currentAlb.copy(title=x))
  case "artist"::x::rs => parseAlbum(rs, currentAlb.copy(artist=x))
  case "date"::x::rs => parseAlbum(rs, currentAlb.copy(date=x))
  case "/album"::rs => currentAlb
  case _::rs => parseAlbum(rs, currentAlb)
  case Nil => currentAlb
}

def parseFile(tokens:List[String]): List[Album] = tokens match {
  case "album"::rs => parseAlbum(rs, Album("","","",Nil))::parseFile(rs)
  case _::rs => parseFile(rs)
  case Nil => Nil
}

println(parseFile(createTokenList(fileContent())))
val albums = parseFile(createTokenList(fileContent()))

def map[A] (input_list: List[A], func: A => A):List[A] = input_list match {
  case Nil => Nil
  case x::xs => func(x)::map(xs,func)
}

map(albums,x => x.copy(title = x.title.toUpperCase))
map(albums,x => {val upperAlbum = x.copy(title = x.title.toUpperCase); upperAlbum.copy(tracks = map(upperAlbum.tracks, x => x.copy(title = x.title.toUpperCase)))})

def poly_map[A,B](input_list: List[A],func: A => B): List[B] = input_list match {
  case Nil => Nil
  case x::xs => func(x)::poly_map(xs,func)
}

poly_map(albums,x => poly_map(x.tracks,y => y.length))

val michaelJackson = albums(1)

def filter[A] (input_list: List[A],condition: A => Boolean) : List[A] = input_list match {
  case Nil => Nil
  case x::xs => if (condition(x)) x::filter(xs,condition) else filter(xs,condition)
}


filter(michaelJackson.tracks, x => x.rating >= 4)
poly_map(filter(michaelJackson.tracks,x=>{
  def contains(writers: List[String]): Boolean = writers match {
    case Nil => false
    case x::xs => if (x == "Rod Temperton") true else contains(xs)
  }
  contains(x.writers)
}),x=>x.title)

def partition[A](input_list:List[A],condition:A=>Boolean):List[List[A]] = input_list match {
  case Nil => Nil::Nil
  case x::xs => if (condition(x)) Nil::partition(xs,condition) else {val partedList = partition(xs,condition); (x::partedList.head)::partedList.tail }
}

partition(michaelJackson.tracks, x=>x.title == "Thriller")
partition(fileContent(),x => x == '<' || x == '>')
poly_map(partition(fileContent(),x => x == '<' || x == '>'),x => x.mkString)
filter(poly_map(partition(fileContent(),x => x == '<' || x == '>'),x => x.mkString),x => !x.trim.isEmpty)

def functionlink(function: Int => Int, link:(Int, Int)=>Int, baseValue: Int, a:Int, b:Int):Int =
  if (a>b) baseValue else link(function(a), functionlink(function, link, baseValue, a+1, b))

def fold(f:(Int,Int) => Int, start: Int, xs: List[Int]) : Int =
  xs match {
    case Nil => start //bei leerer Liste Rückgabe von start
    case h::ts => fold(f,f(start,h),ts)
  }

def range(a:Int,b:Int) : List[Int] = if (a>b) Nil else a::range(a+1,b)

def linkWithFold(function: Int => Int, link:(Int, Int)=>Int, baseValue: Int, a:Int, b:Int) = fold(link,baseValue,map(range(a,b),function))

functionlink(x=>x,(x,y)=>x+y,0,1,4)

linkWithFold(x=>x,(x,y)=>x+y,0,1,4)
