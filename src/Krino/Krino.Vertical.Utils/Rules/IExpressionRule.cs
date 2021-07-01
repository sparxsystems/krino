using System;
using System.Linq.Expressions;

namespace Krino.Vertical.Utils.Rules
{
    public interface IExpressionRule<T> : IRule<T>
    {
        Expression<Func<T, bool>> Expression { get; }
    }
}
