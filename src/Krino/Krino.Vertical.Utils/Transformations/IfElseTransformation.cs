using Krino.Vertical.Utils.Rules;
using System;

namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// Conditional transformation.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class IfElseTransformation<T> : ITransformation<T>
    {
        internal static IfElseTransformation<T> Else(IfElseTransformation<T> provider, ITransformation<T> elseTransformation)
        {
            provider.myElseTransformation = elseTransformation;
            return provider;
        }

        private IRule<T> myRule;
        private ITransformation<T> myTransformation;
        private ITransformation<T> myElseTransformation;

        public IfElseTransformation(IRule<T> rule, ITransformation<T> transformation, ITransformation<T> elseTransformation)
        {
            myRule = rule;
            myTransformation = transformation;
            myElseTransformation = elseTransformation;
        }

        public IRule<T> Rule => myRule;

        /// <summary>
        /// If the rule is true then it performes the transformation otherwise it performs the 'else' transformation if not null.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public T Transform(T value)
        {
            if (myRule == null)
            {
                throw new InvalidOperationException("Failed to perform the conditional transformation because the condition is not defined.");
            }
            if (myTransformation == null)
            {
                throw new InvalidOperationException("Failed to perform the conditional transformation because the transformation is not defined.");
            }

            T result;

            if (myRule.Evaluate(value))
            {
                result = myTransformation.Transform(value);
            }
            else if (myElseTransformation != null)
            {
                result = myElseTransformation.Transform(value);
            }
            else
            {
                result = value;
            }

            return result;
        }

        public bool Equals(ITransformation<T> other) =>
            other is IfElseTransformation<T> otherTransformation &&
            myRule.Equals(otherTransformation.myRule) &&
            myTransformation.Equals(otherTransformation.myTransformation) &&
            (myElseTransformation == null && otherTransformation.myElseTransformation == null ||
             myElseTransformation != null && otherTransformation.myElseTransformation != null &&
             myElseTransformation.Equals(otherTransformation.myElseTransformation));
    }
}
