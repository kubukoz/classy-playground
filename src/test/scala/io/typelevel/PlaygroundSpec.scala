package io.typelevel
import cats.{Applicative, FlatMap}
import cats.data.{EitherT, Reader, ReaderT}
import cats.effect.{IO, Sync}
import cats.effect.concurrent.Ref
import org.scalatest.{Assertion, FlatSpec, Matchers}
import Functions._
import cats.implicits._
import cats.mtl.ApplicativeAsk
import cats.mtl.implicits._
import imps._

import scala.language.higherKinds

object implicit0 {
  def unapply[A](a: A): Option[A] = Some(a)
}

class PlaygroundSpec extends FlatSpec with Matchers {
  "makeNetworkCall" should "use its config" in {
    def program[F[_]: FlatMap: ApplicativeAsk[?[_], NetworkConfig]: Sync]: F[Assertion] = {
      def makeCanNetwork(pings: Ref[F, List[String]]): CanNetwork[F] =
        protocol =>
          pings.modify { pastPings => (protocol :: pastPings, s"New state: ${pastPings.size + 1} pings")
        }

      def makeLogger(logs: Ref[F, List[String]]): Logger[F] = s => logs.update(s :: _)

      def makeConsole(prints: Ref[F, List[String]]): Console[F] = s => prints.update(s :: _)

      val stringNilRef = Ref[F].of(List.empty[String])

      for {
        initConfig <- ApplicativeAsk[F, NetworkConfig].ask

        pings  <- stringNilRef
        logs   <- stringNilRef
        prints <- stringNilRef

        implicit0(cnn: CanNetwork[F])  = makeCanNetwork(pings)
        implicit0(logger: Logger[F])   = makeLogger(logs)
        implicit0(console: Console[F]) = makeConsole(prints)

        result1 <- makeNetworkCall[NetworkError, NetworkConfig, F](amount = 100)
        result2 <- makeNetworkCall[NetworkError, NetworkConfig, F](amount = 20)

        pingsAfter <- pings.get
        logsAfter  <- logs.get
      } yield {
        result1 shouldBe "New state: 1 pings"
        result2 shouldBe "New state: 2 pings"

        pingsAfter shouldBe List("tcp", "tcp")

        logsAfter shouldBe List(
          s"Read $initConfig from net config (whole config $initConfig)",
          s"Read $initConfig from net config (whole config $initConfig)"
        )
      }
    }

    type F[A] = ReaderT[IO, NetworkConfig, A]

    program[F].run(NetworkConfig(4, "tcp")).unsafeRunSync()
  }
}
