using System.Collections.Generic;

namespace Krino.Vertical.Utils.Rules
{
    public class ContainsRule<T> : RuleBase<T>, ISetRule<T>
    {
        public ContainsRule(IEnumerable<T> items, IEqualityComparer<T> comparer = null)
        {
            Items = new HashSet<T>(items, comparer);
        }

        public ISet<T> Items { get; private set; }

        public override bool Evaluate(T value) => Items.Contains(value);


        public override bool Equals(IRule<T> other) => other is ContainsRule<T> otherSet && Items.SetEquals(otherSet.Items);
    }
}
