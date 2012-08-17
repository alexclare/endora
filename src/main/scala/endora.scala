import scala.collection.mutable.ArrayBuffer

import s7.sensation._
import s7.sensation.playlist._
import s7.sensation.artist.{Artist, Name}
import s7.sensation.song.{Search, Song, Title}

object Endora extends App {
  val usage = "java-run-stuff [-s artist seed]* [-a artist seed]*\n  The first 5 artists/songs will be used to generate your station!"

  val keys = """
help = ?
favorite song = +
ban song = -
skip song = n
favorite artist = '
ban artist = ;
quit = q"""

  if (args.length < 2) {
    Console.println(usage)
    System.exit(-2)
  }

  implicit val apiKey = EchoNestKey(System.getenv("EN_API_KEY") match {
    case s:String => s
    case _ => Console.println(usage); System.exit(-1); ""
  })

  // Not really any error checking for the seeds
  val seeds = ArrayBuffer.empty[PlaylistSeed]
  var group = ""
  var accum = ""
  for (arg <- args) {
    if (arg == "-s" || arg == "-a") {
      group match {
        case "-a" => seeds += Artist(Name -> accum)
        case "-s" => seeds += Song.search(Search.Title -> accum).head
        case _    =>
      }
      group = arg
      accum = ""
    } else {
      accum += arg
    }
    Console.println("" + group + " - " + accum)
  }
  group match {
    case "-a" => seeds += Artist(Name -> accum)
    case "-s" => seeds += Song.search(Search.Title -> accum).head
  }

  if (seeds.length < 1) {
    Console.println(usage)
    System.exit(-2)
  }
  val list = playlist.Dynamic(seeds)
  var description = ""
  Console.println(keys)
  while(true) {
    try {
      val (s, fb) = list.next
      Console.println(s.apply(Title) + " - Artist")
      while (description.size == 0) {
        Console.readChar match {
          case '+'  => fb(FavoriteSong); description = "  Made a favorite"
          case '-'  => fb(BanSong); description = "  Banned"
          case 'n'  => fb(SkipSong); description = "  Skipped"
          case '\'' => fb(FavoriteArtist); description = "  Made the artist a favorite"
          case ';'  => fb(BanArtist); description = "  Banned the artist"
          case 'q'  => list.delete; System.exit(0)
          case _    => Console.println(keys)
        }
      }
      Console.println(description)
      description = ""
    } catch {
      // Needs to get feedback or be steered or the list will just repeat
      case ex: NoSuchElementException =>
        list.restart(seeds)
    }
  }
}
