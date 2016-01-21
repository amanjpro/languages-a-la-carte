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
      "A Class cannot extend an interface, it can only implement it"
  }

  case object EXTENDING_AN_INTERFACE extends ErrorCode {
    val message: String =
      "Classes may not be implemented, but extended"
  }

  case object CLASS_SHOULD_EXTEND_EXACTlY_ONE_CLASS extends ErrorCode {
    val message: String =
      "A class should extend exactly one class"
  }

  case object CONSTRUCTOR_SHOULD_HAVE_THE_SAME_TYPE_AS_CONTAINING_CLASS extends ErrorCode {
    val message: String =
      "Constructors should have the same type as their containing class"
  }

  case object FIELD_OWNED_BY_NON_CLASS extends ErrorCode {
    val message: String =
      "Field can only appear in class bodies"
  }

  case object PARAM_OWNED_BY_NON_METHOD extends ErrorCode {
    val message: String =
      "Parameter can only appear in method parameter contexts"
  }

  case object LOCAL_VARIABLE_OWNED_BY_NON_LOCAL extends ErrorCode {
    val message: String =
      "Local variable can only appear in local contexts"
  }

  case object EXPLICIT_CONSTRUCTOR_INVOKATION_NOT_FIRST_STATEMENT extends
      ErrorCode {
    val message: String =
      "Explicit constructor invokation needs to be the first statement"
  }

  case object EXPLICIT_CONSTRUCTOR_INVOKATION_IN_METHOD extends
      ErrorCode {
    val message: String =
      "Explicit constructor invokation can only appear in constructors"
  }

  case object OVERRIDING_FINAL_METHOD extends
      ErrorCode {
    val message: String =
      "Final methods may not be overridden"
  }

  case object CLASS_ALREADY_DEFINED extends
      ErrorCode {
    val message: String =
      "Class already defined"
  }

  case object FINAL_PARENT extends
      ErrorCode {
    val message: String =
      "A parent class/interface must not be final"
  }


  case object ABSTRACT_FINAL extends ErrorCode {
    val message: String =
      "Abstract classes, abstract methods and interfaces cannot be final"
  }

  case object PUBLIC_CLASS_FILE_NAME_MATCH_ERROR extends ErrorCode {
    val message: String =
      "Public classes/interfaces should have the same name as the containing file"
  }

  case object REFERENCE_FIELD_BEFORE_SUPERTYPE extends ErrorCode {
    val message: String =
      "Cannot reference a field before supertype constructor has been called"
  }

  case object FIELD_FORWARD_REFERENCE_IN_STATIC_INIT extends ErrorCode {
    val message: String =
      "Accessing fields (before declaration) in static initializer"
  }
}

object ErrorCodes extends ErrorCodes

