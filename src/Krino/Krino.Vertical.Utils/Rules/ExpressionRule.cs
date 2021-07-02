using System;
using System.Linq.Expressions;

namespace Krino.Vertical.Utils.Rules
{
    public class ExpressionRule<T> : RuleBase<T>, IExpressionRule<T>
    {
        private Func<T, bool> myCompiledExpression;

        public ExpressionRule(Expression<Func<T, bool>> expression)
        {
            Expression = expression;
            myCompiledExpression = Expression.Compile();
        }

        public Expression<Func<T, bool>> Expression { get; private set; }

        public override bool Evaluate(T value) => myCompiledExpression(value);


        public override bool Equals(IRule<T> other) => other is ExpressionRule<T> expressionRule && Expression.ToString() == expressionRule.Expression.ToString();

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ Expression.GetHashCode();

            return hash;
        }
    }
}
