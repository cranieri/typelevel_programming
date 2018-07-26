package test

import scala.util.{Failure, Success, Try}

object PaymentSubmissionService {

  type ValidatePaymentSubmission = UnvalidatedPaymentSubmission => Try[ValidatedPaymentSubmission[Valid.type]]
  type SanitisePaymentSubmission = ValidatedPaymentSubmission[Valid.type] => Try[SanitisedPaymentSubmission[Sanitised.type]]

  val validatePaymentSubmission: ValidatePaymentSubmission = {
    (paymentSubmission: UnvalidatedPaymentSubmission) => Try(ValidatedPaymentSubmission[Valid.type](paymentSubmission.amount, "ref", Valid))

  }

  val sanitisePaymentSubmission: SanitisePaymentSubmission = {
    (paymentSubmission: ValidatedPaymentSubmission[Valid.type]) => {
      Try(SanitisedPaymentSubmission(paymentSubmission.amount, "ref", Sanitised))
    }
  }

  val paymentSubmission = UnvalidatedPaymentSubmission(1000, "ref1", Unvalidated)
  val sanitisedPaymentSubmission: Try[SanitisedPaymentSubmission[Sanitised.type]] = for {
    validatedPaymentSubmission <- validatePaymentSubmission(paymentSubmission)
    sanitisedPaymentSubmission <- sanitisePaymentSubmission(validatedPaymentSubmission)
  } yield sanitisedPaymentSubmission

  val submitPayment = sanitisedPaymentSubmission match {
    case Success(SanitisedPaymentSubmission(_,_,status)) => status
    case Failure(exception) => exception
  }
}

object WebServer extends App with StatusRoutes {
  println(PaymentSubmissionService.submitPayment.toString)
}

