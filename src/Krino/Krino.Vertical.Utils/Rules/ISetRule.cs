using System.Collections.Generic;

namespace Krino.Vertical.Utils.Rules
{
    public interface ISetRule<T> : IRule<T>
    {
        public ISet<T> Items { get; }
    }
}
