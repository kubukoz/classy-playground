package io.typelevel
import java.time.LocalDate

import cats.data.{EitherT, Kleisli}
import cats.effect._
import cats.implicits._
import cats.mtl.{ApplicativeAsk, FunctorRaise}
import cats.mtl.implicits._
import cats.{FlatMap, Functor, MonadError, Show}
import io.typelevel.DbError.BadConnection
import io.typelevel.Functions.application
import io.typelevel.NetworkError.InvalidHost
import monocle.macros.{GenLens, GenPrism}
import monocle.{Lens, Prism}
import simulacrum._

import scala.language.{higherKinds, implicitConversions}

case class DbConfig(port: Int, url: String)
case class NetworkConfig(retries: Int, protocol: String)

@typeclass
trait HasDbConfig[S] {
  def dbConfig(s: S): DbConfig
  def port(s: S): Int   = dbConfig(s).port
  def url(s: S): String = dbConfig(s).url
}

object HasDbConfig {
  implicit val dcInstance: HasDbConfig[DbConfig] = x => x

  def composeLens[S, A: HasDbConfig](lens: Lens[S, A]): HasDbConfig[S] = {
    import ops._
    lens.get(_).dbConfig
  }
}

@typeclass
trait HasNetworkConfig[S] {
  def networkConfig(s: S): NetworkConfig
  def retries(s: S): Int     = networkConfig(s).retries
  def protocol(s: S): String = networkConfig(s).protocol
}

object HasNetworkConfig {
  implicit val ncInstance: HasNetworkConfig[NetworkConfig] = x => x

  def composeLens[S, A: HasNetworkConfig](lens: Lens[S, A]): HasNetworkConfig[S] = {
    import ops._
    lens.get(_).networkConfig
  }
}

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
trait AsDbError[E] {
  def asDbError(e: E): Option[DbError]
  def fromDbError(dbError: DbError): E
}

object AsDbError {
  implicit val deInstance: AsDbError[DbError] = new AsDbError[DbError] {
    override def asDbError(e: DbError): Option[DbError] = Some(e)
    override def fromDbError(e: DbError): DbError       = e
  }

  def composePrism[E, A: AsDbError](prism: Prism[E, A]): AsDbError[E] = {
    import ops._
    new AsDbError[E] {
      override def asDbError(e: E): Option[DbError] = prism.getOption(e).flatMap(_.asDbError)
      override def fromDbError(dbError: DbError): E = prism.reverseGet(AsDbError[A].fromDbError(dbError))
    }
  }
}

@typeclass
trait AsNetworkError[E] {
  def asNetworkError(e: E): Option[NetworkError]
  def fromNetworkError(networkError: NetworkError): E
}

object AsNetworkError {
  implicit val neInstance: AsNetworkError[NetworkError] = new AsNetworkError[NetworkError] {
    override def asNetworkError(e: NetworkError): Option[NetworkError]      = Some(e)
    override def fromNetworkError(networkError: NetworkError): NetworkError = networkError
  }

  def composePrism[E, A: AsNetworkError](prism: Prism[E, A]): AsNetworkError[E] = {
    import ops._
    new AsNetworkError[E] {
      override def asNetworkError(e: E): Option[NetworkError] = prism.getOption(e).flatMap(_.asNetworkError)
      override def fromNetworkError(networkError: NetworkError): E =
        prism.reverseGet(AsNetworkError[A].fromNetworkError(networkError))
    }
  }
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
  implicit def canDbSync[F[_]: Sync: FunctorRaise[?[_], E], E: AsDbError]: CanDb[F] = new CanDb[F] {
//    override val select: F[Int] = FunctorRaise[F, E].raise(AsDbError[E].fromDbError(DbError.BadConnection))
    override val select: F[Int] = Sync[F].delay(println("selecting from db...")).as(1000)
  }
}

@typeclass
trait CanNetwork[F[_]] {
  def ping(protocol: String): F[String]
}

object CanNetwork {
  implicit def canNetworkSync[F[_]: Sync: FunctorRaise[?[_], E], E: AsNetworkError]: CanNetwork[F] = new CanNetwork[F] {
//    override def ping(protocol: String): F[String] = FunctorRaise[F, E].raise(AsNetworkError[E].fromNetworkError(NetworkError.InvalidHost))
    override def ping(protocol: String): F[String] = Sync[F].delay(println(s"pinging using $protocol...")).as("wohoo")
  }
}

object imps
    extends AsDbError.ToAsDbErrorOps
    with AsNetworkError.ToAsNetworkErrorOps
    with HasDbConfig.ToHasDbConfigOps
    with HasNetworkConfig.ToHasNetworkConfigOps

object Functions {
  import imps._

  def makeDbCall[E: AsDbError, R: HasDbConfig, F[_]: ApplicativeAsk[?[_], R]: FlatMap: Logger: CanDb]: F[Int] =
    for {
      config   <- ApplicativeAsk[F, R].ask.map(_.dbConfig)
      _        <- Logger[F].info(s"Read $config from db config")
      response <- CanDb[F].select
    } yield response

  def makeNetworkCall[E: AsNetworkError,
                      R: HasNetworkConfig,
                      F[_]: FlatMap: CanNetwork: ApplicativeAsk[?[_], R]: Logger : Console](amount: Int): F[String] =
    for {
      config   <- ApplicativeAsk[F, R].ask
      _        <- Logger[F].info(s"Read ${config.networkConfig} from net config (whole config $config)")
      _        <- Console[F].print("Using amount:")
      _        <- Console[F].print(amount)
      response <- CanNetwork[F].ping(config.protocol)
    } yield response

  def application[E: AsNetworkError: AsDbError,
                  R: HasNetworkConfig: HasDbConfig,
                  F[_]: CanDb: CanNetwork: FlatMap: ApplicativeAsk[?[_], R]: Logger : Console]: F[String] =
    makeDbCall[E, R, F].flatMap(makeNetworkCall[E, R, F])
}

object Playground extends IOApp {
  case class AppConfig(dbConfig: DbConfig, networkConfig: NetworkConfig)

  object AppConfig {
    implicit val hasDbConfig: HasDbConfig[AppConfig] = HasDbConfig.composeLens(GenLens[AppConfig](_.dbConfig))
    implicit val hasNetworkConfig: HasNetworkConfig[AppConfig] =
      HasNetworkConfig.composeLens(GenLens[AppConfig](_.networkConfig))
  }

  sealed trait AppError extends Product with Serializable

  object AppError {
    case class Db(error: DbError)       extends AppError
    case class Net(error: NetworkError) extends AppError

    implicit val asDbError: AsDbError[AppError] = {
      val thePrism = GenPrism[AppError, Db].andThen(Prism[Db, DbError](_.error.some)(Db.apply))

      AsDbError.composePrism(thePrism)
    }

    implicit val asNetworkError: AsNetworkError[AppError] = {
      val thePrism = GenPrism[AppError, Net].andThen(Prism[Net, NetworkError](_.error.some)(Net.apply))

      AsNetworkError.composePrism(thePrism)
    }
  }

  type App[A] = Kleisli[EitherT[IO, AppError, ?], AppConfig, A]

  val appMain: App[Unit] = application[AppError, AppConfig, App].flatMap(Console[App].putStrLn)

  val appToIO: App[Unit] => IO[ExitCode] =
    _.run(AppConfig(DbConfig(8080, "localhost"), NetworkConfig(42, "https"))).value.flatMap {
      case Left(error) => Console[IO].putStrLn("Encountered error: " + error.toString).as(ExitCode.Error)
      case Right(_)    => IO.pure(ExitCode.Success)
    }

  override def run(args: List[String]): IO[ExitCode] =
    appToIO(appMain)
}
