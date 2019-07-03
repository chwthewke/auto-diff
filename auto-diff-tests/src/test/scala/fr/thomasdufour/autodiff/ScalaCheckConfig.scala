package fr.thomasdufour.autodiff

import org.scalatest.prop.Configuration

trait ScalaCheckConfig extends Configuration {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration( minSuccessful = 100 )

}
