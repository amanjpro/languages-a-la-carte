package ch.usi.inf.l3.sana.ooj.errors

import ch.usi.inf.l3.sana
import sana.tiny.errors.ErrorCode
import sana.brokenj


trait ErrorCodes extends brokenj.errors.ErrorCodes {
  case object ACCESSING_THIS_IN_STATIC extends ErrorCode {
    val message: String =
      "``this'' cannot be accessed inside a static context"
  }

  case object ACCESSING_THIS_OUTSIDE_A_CLASS extends ErrorCode {
    val message: String =
      "``this'' needs to be enclosed by classes, either directly or indirectly"
  }


  case object ACCESSING_SUPER_OUTSIDE_A_CLASS extends ErrorCode {
    val message: String =
      """|``super'' needs to be enclosed by classes,
         |either directly or indirectly""".stripMargin
  }

  case object ACCESSING_SUPER_IN_STATIC extends ErrorCode {
    val message: String =
      "``super'' cannot be accessed inside a static context"
  }

  case object ACCESSING_SUPER_IN_OBJECT_CLASS extends ErrorCode {
    val message: String =
      "``super'' cannot be accessed inside Object class"
  }


  case object AMBIGUOUS_METHOD_INVOCATION extends ErrorCode {
    val message: String = "Method invocation is ambiguous"
  }


  case object INSTANCE_METHOD_IN_STATIC_CONTEXT_INVOK extends ErrorCode {
    val message: String = "Invoking an instance method in a static context"
  }

  case object INSTANCE_FIELD_IN_STATIC_CONTEXT_INVOK extends ErrorCode {
    val message: String = "Accessing an instance field in a static context"
  }


  case object FIELD_NOT_ACCESSIBLE extends ErrorCode {
    val message: String = "Field is not accessible from this context"
  }



  case object NON_STATIC_FIELD_IN_INTERFACE extends ErrorCode {
    val message: String = "Fields in interfaces should be static final"
  }

  case object NON_FINAL_FIELD_IN_INTERFACE extends ErrorCode {
    val message: String = "Fields in interfaces should be static final"
  }


  case object NON_ABSTRACT_METHOD_IN_INTERFACE extends ErrorCode {
    val message: String = "Method is not abstract"
  }

  case object CONSTRUCTOR_IN_INTERFACE extends ErrorCode {
    val message: String = "Interfaces cannot have constructors"
  }

  case object ABSTRACT_METHOD_CANNOT_HAVE_BODY extends ErrorCode {
    val message: String = "Abstract methods must not have body"
  }

  case object CONSTRUCTOR_CANNOT_BE_ABSTRACT extends ErrorCode {
    val message: String = "Constructors cannot be abstract"
  }

  case object ABSTRACT_METHOD_IN_CONCRETE_CLASS extends ErrorCode {
    val message: String = "Abstract methods cannot occur in concrete classes"
  }

  case object INSTANTIATING_NON_CONCRETE_CLASS extends ErrorCode {
    val message: String =
      "Abstract classes/interfaces cannot be instantiated"
  }

  case object NON_IMPLEMENTED_METHODS extends ErrorCode {
    val message: String =
      "Concrete classes cannot have abstract members"
  }

  case object IMPLEMENTING_A_CLASS extends ErrorCode {
    val message: String =
      "Implement cannot extend an interface, it can only implement it"
  }

  case object EXTENDING_AN_INTERFACE extends ErrorCode {
    val message: String =
      "Classes may not be implemented, but extended"
  }

  case object CLASS_SHOULD_EXTEND_EXACTlY_ONE_CLASS extends ErrorCode {
    val message: String =
      "A class should extend exactly one class"
  }
}

object ErrorCodes extends ErrorCodes

