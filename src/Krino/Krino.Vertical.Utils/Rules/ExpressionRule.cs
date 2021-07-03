using System;
using System.Linq.Expressions;

namespace Krino.Vertical.Utils.Rules
{
    public class ExpressionRule<T> : RuleBase<T>, IExpressionRule<T>
    {
        private int myHashCode;
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
            if (myHashCode == 0)
            {
                myHashCode = 486187739;
                myHashCode = (myHashCode * 16777619) ^ Expression.ToString().GetHashCode();
            }

            return myHashCode;
        }
    }
}
