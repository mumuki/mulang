module Mulang
  module Sexp

    # Basic S-expression

    def ms(tag, *contents)
      if contents.empty?
        {tag: tag}
      elsif contents.size == 1
        {tag: tag, contents: contents.first}
      else
        {tag: tag, contents: contents}
      end
    end

    # Basic elements

    def primitive(operator)
      ms(:Primitive, operator)
    end

    def sequence(*contents)
      if contents.empty?
        none
      elsif contents.size == 1
        contents[0]
      else
        ms(:Sequence, *contents)
      end
    end

    def none
      ms(:None)
    end

    # Callables

    def simple_function(name, args, body)
      callable :Function, name, args, body
    end

    def simple_method(name, args, body)
      callable :Method, name, args, body
    end

    def primitive_method(name, args, body)
      callable :PrimitiveMethod, name, args, body
    end

    def callable(type, name, args, body)
      {
          tag: type,
          contents: [
              name,
              [
                  [ args, {tag: :UnguardedBody, contents: body }]
              ]
          ]
      }
    end

    # Applications

    def primitive_send(sender, op, args)
      ms(:Send, sender, primitive(op), args)
    end

    def simple_send(sender, message, args)
      ms(:Send, sender, ms(:Reference, message), args)
    end

    def application(name, args)
      ms :Application, [ms(:Reference, name), args]
    end

    def binary_application(operator, left, right)
      application operator, [left, right]
    end
  end
end
