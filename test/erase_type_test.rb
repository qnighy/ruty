# frozen_string_literal: true

require_relative "test_helper"
require "ruty"

class EraseTypeTest < Minitest::Test
  def test_erase_type_simple
    assert_equal "x", Ruty.erase_type("x")
  end

  def test_erase_type_local_var
    assert_equal "x           = 42", Ruty.erase_type("x @ Integer = 42")
  end
end
