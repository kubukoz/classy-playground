package io.typelevel

import java.time.LocalDate

import cats.data.{EitherT, Kleisli}
import cats.effect._
import cats.implicits._
import cats.mtl.instances.all._
import cats.mtl.{ApplicativeAsk, FunctorRaise}
import cats.{FlatMap, Show}
import io.typelevel.Functions.application
import simulacrum._

import scala.language.{higherKinds, implicitConversions}
import com.olegpy.meow.hierarchy._
import io.typelevel.Playground.AppConfig

case class DbConfig(port: Int, url: String)
case class NetworkConfig(retries: Int, protocol: String)

sealed trait DbError extends Product with Serializable

object DbError {
  case class DataCorrupted(message: String) extends DbError
  case object BadConnection                 extends DbError
}

sealed trait NetworkError extends Product with Serializable

object NetworkError {
  case object InvalidHost  extends NetworkError
  case object NoConnection extends NetworkError
}

@typeclass
trait Logger[F[_]] {
  def info(s: String): F[Unit]
}

object Logger {
  implicit def liftInstance[F[_]: Sync]: Logger[F] = new Logger[F] {
    override def info(s: String): F[Unit] = Sync[F].delay(println(s"Logger @ ${LocalDate.now()}: " + s))
  }
}

@typeclass
trait Console[F[_]] {
  def putStrLn(s: String): F[Unit]
  def print[T: Show](t: T): F[Unit] = putStrLn(t.show)
}

object Console {
  implicit def liftInstance[F[_]: Sync]: Console[F] = s => Sync[F].delay(println(s))
}

@typeclass
trait CanDb[F[_]] extends AnyRef {
  val select: F[Int]
}

object CanDb {
  implicit def canDbSync[F[_]: Sync: FunctorRaise[?[_], DbError]]: CanDb[F] = new CanDb[F] {
    override val select: F[Int] = FunctorRaise.raise(DbError.BadConnection: DbError)
//    override val select: F[Int] = Sync[F].delay(println("selecting from db...")).as(1000)
  }
}

@typeclass
trait CanNetwork[F[_]] {
  def ping(protocol: String): F[String]
}

object CanNetwork {
  implicit def canNetworkSync[F[_]: Sync: FunctorRaise[?[_], NetworkError]]: CanNetwork[F] = new CanNetwork[F] {
//    override def ping(protocol: String): F[String] = FunctorRaise[F, NetworkError].raise(NetworkError.InvalidHost)
    override def ping(protocol: String): F[String] = Sync[F].delay(println(s"pinging using $protocol...")).as("wohoo")
  }
}

object Functions {

  def makeDbCall[F[_]: ApplicativeAsk[?[_], DbConfig]: FlatMap: Logger: CanDb]: F[Int] =
    for {
      config   <- ApplicativeAsk[F, DbConfig].ask
      _        <- Logger[F].info(s"Read $config from db config")
      response <- CanDb[F].select
    } yield response

  def makeNetworkCall[F[_]: FlatMap: CanNetwork: ApplicativeAsk[?[_], NetworkConfig]: Logger: Console](
    amount: Int): F[String] =
    for {
      config   <- ApplicativeAsk[F, NetworkConfig].ask
      _        <- Logger[F].info(s"Read $config from net config (whole config $config)")
      _        <- Console[F].print("Using amount:")
      _        <- Console[F].print(amount)
      response <- CanNetwork[F].ping(config.protocol)
    } yield response

  def application[F[_]: CanDb: CanNetwork: FlatMap: ApplicativeAsk[?[_], AppConfig]: Logger: Console]: F[String] =
    makeDbCall[F].flatMap(makeNetworkCall[F])
}

object Playground extends IOApp {
  case class AppConfig(dbConfig: DbConfig, networkConfig: NetworkConfig)

  sealed trait AppError extends Product with Serializable

  object AppError {
    case class Db(error: DbError)       extends AppError
    case class Net(error: NetworkError) extends AppError
  }

  type App[A] = Kleisli[EitherT[IO, AppError, ?], AppConfig, A]

  //abstraction layer that makes derivation of CanDb, CanNetwork, etc. (using derivation of FunctorRaise[F, DbError]) happen
  def appF[F[_]: Sync: FlatMap: ApplicativeAsk[?[_], AppConfig]: FunctorRaise[?[_], AppError]: Logger: Console]
    : F[String] = application[F]

  val appMain: App[Unit] = appF[App].flatMap(Console[App].putStrLn)

  val appToIO: App[Unit] => IO[ExitCode] =
    _.run(AppConfig(DbConfig(8080, "localhost"), NetworkConfig(42, "https"))).value.flatMap {
      case Left(error) => Console[IO].putStrLn("Encountered error: " + error.toString).as(ExitCode.Error)
      case Right(_)    => IO.pure(ExitCode.Success)
    }

  override def run(args: List[String]): IO[ExitCode] =
    appToIO(appMain)
}
