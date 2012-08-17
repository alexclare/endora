import scala.collection.mutable.ArrayBuffer

import s7.sensation._
import s7.sensation.playlist._
import s7.sensation.artist.{Artist, Name}
import s7.sensation.song.{Search, Song, Title}

object Endora extends App {
  val usage = "java-run-stuff [-s artist seed]* [-a artist seed]*\n  The first 5 artists/songs will be used to generate your station!"

  val keys = """Key map:
help = ?
favorite song = +
ban song = -
skip song = n
favorite artist = '
ban artist = ;
quit = q
listen = anything else
"""

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
    } else accum += arg
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
  list.steer(Variety(0))
  var description = ""
  Console.println(keys)
  while(true) {
    try {
      val (s, fb) = list.next
      Console.println(s.apply(Title) + " - Artist")
      while (description.size == 0) {
        Console.readChar match {
          case '+'  => {
            fb(FavoriteSong)
            list.steer(PlaySimilar(5))
            description = "  Made a favorite"
          }
          case '-'  => {
            fb(BanSong)
            list.steer(PlaySimilar(-5))
            description = "  Banned"
          }
          case 'n'  => fb(SkipSong); description = "  Skipped"
          case '\'' => {
            fb(FavoriteArtist)
            list.steer(PlaySimilar(1));
            description = "  Made the artist a favorite"
          }
          case ';'  => {
            fb(BanArtist)
            list.steer(PlaySimilar(-1))
            description = "  Banned the artist"
          }
          case 'q'  => list.delete; System.exit(0)
          case '?'  => Console.println(keys)
          case _    => description = "  Listened"
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
