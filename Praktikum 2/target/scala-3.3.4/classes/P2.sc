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