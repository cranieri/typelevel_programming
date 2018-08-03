package test

import scala.util.{Failure, Success, Try}

object PaymentSubmissionService2 {

  type ValidatePaymentSubmission = UnvalidatedPaymentSubmission => Try[ValidatedPaymentSubmission[Valid.type]]
  type SanitisePaymentSubmission = ValidatedPaymentSubmission[Valid.type] => Try[SanitisedPaymentSubmission[Sanitised.type]]

  val validatePaymentSubmission: ValidatePaymentSubmission = {
    (paymentSubmission: UnvalidatedPaymentSubmission) => Try(ValidatedPaymentSubmission[Valid.type](paymentSubmission.amount, "ref", Valid))

  }

  val sanitisePaymentSubmission: SanitisePaymentSubmission = {
    (paymentSubmission: ValidatedPaymentSubmission[Valid.type]) =>
      Try(SanitisedPaymentSubmission(paymentSubmission.amount, "ref", Sanitised))

  }

  val paymentSubmission = UnvalidatedPaymentSubmission(1000, "ref1", Unvalidated)
  val sanitisedPaymentSubmission: Try[SanitisedPaymentSubmission[Sanitised.type]] = for {
    validatedPaymentSubmission <- validatePaymentSubmission(paymentSubmission)
    sanitisedPaymentSubmission <- sanitisePaymentSubmission(validatedPaymentSubmission)
  } yield sanitisedPaymentSubmission

  val submitPayment1 = sanitisedPaymentSubmission match {
    case Success(SanitisedPaymentSubmission(_, _, status)) => status
    case Failure(exception) => exception
  }
}

object PaymentSubmissionService3 {
  def successfullySanitized = true
  def successfullyValidated = true

//  sealed trait PaymentSubmissionStatus
//  case class ValidatedPaymentSubmission(amount: Int, reference: String, status: String) extends PaymentSubmissionStatus
//  case class SanitisedPaymentSubmission(amount: Int, reference: String, status: String) extends PaymentSubmissionStatus
//  case class UnvalidatedPaymentSubmission(amount: Int, reference: String, status: String) extends PaymentSubmissionStatus

  def validatePaymentSubmission(unvalidatedPaymentSubmission: UnvalidatedPaymentSubmission): ValidatedPaymentSubmission[ValidatedPaymentStatus] = {
    if (successfullyValidated)
      ValidatedPaymentSubmission(paymentSubmission.amount, "ref", Valid)
    else
      ValidatedPaymentSubmission(paymentSubmission.amount, "ref", Invalid)
  }

  def sanitisePaymentSubmission(validatedPaymentSubmission: ValidatedPaymentSubmission[ValidatedPaymentStatus]): SanitisedPaymentSubmission[SanitisedPaymentStatus] = {
    if (successfullySanitized)
      SanitisedPaymentSubmission(paymentSubmission.amount, "ref", Sanitised)
    else
      SanitisedPaymentSubmission(paymentSubmission.amount, "ref", NotSanitised)
  }

  val paymentSubmission = UnvalidatedPaymentSubmission(1000, "ref1", Unvalidated)

  val sanitisedPaymentSubmission = sanitisePaymentSubmission(validatePaymentSubmission(paymentSubmission))

//  val sanitisedPaymentSubmission: Try[SanitisedPaymentSubmission[Sanitised.type]] = for {
//    validatedPaymentSubmission <- validatePaymentSubmission(paymentSubmission)
//    sanitisedPaymentSubmission <- sanitisePaymentSubmission(validatedPaymentSubmission)
//  } yield sanitisedPaymentSubmission
}

object PaymentSubmissionService {
  val validatePaymentSubmission: UnvalidatedPaymentSubmission => Try[ValidatedPaymentSubmission[Valid.type]] = {
    (paymentSubmission: UnvalidatedPaymentSubmission) => Try(ValidatedPaymentSubmission[Valid.type](paymentSubmission.amount, "ref", Valid))
  }

  val sanitisePaymentSubmission: ValidatedPaymentSubmission[Valid.type] => Try[SanitisedPaymentSubmission[Sanitised.type]] = {
    (paymentSubmission: ValidatedPaymentSubmission[Valid.type]) => {
      Try(SanitisedPaymentSubmission(paymentSubmission.amount, "ref", Sanitised))
    }
  }

  val submitPaymentToStarling: SanitisedPaymentSubmission[Sanitised.type] => Try[SentPaymentSubmission] = {
    (paymentSubmission: SanitisedPaymentSubmission[Sanitised.type]) => {
      Try(SentPaymentSubmission(paymentSubmission.amount, "ref", Submitted))
    }
  }

  val paymentSubmission = UnvalidatedPaymentSubmission(1000, "ref1", Unvalidated)
  val sanitisedPaymentSubmission: Try[SanitisedPaymentSubmission[Sanitised.type]] = for {
    validatedPaymentSubmission <- validatePaymentSubmission(paymentSubmission)
    sanitisedPaymentSubmission <- sanitisePaymentSubmission(validatedPaymentSubmission)
  } yield sanitisedPaymentSubmission

  val submitPayment = sanitisedPaymentSubmission match {
    case Success(SanitisedPaymentSubmission(_, _, status)) => status
    case Failure(exception) => exception
  }
}

object WebServer extends App with StatusRoutes {
  println(PaymentSubmissionService.submitPayment.toString)
}

