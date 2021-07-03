using Krino.Vertical.Utils.Collections;
using System.Collections.Generic;

namespace Krino.Vertical.Utils.Rules
{
    public class ContainsRule<T> : RuleBase<T>, ISetRule<T>
    {
        private int myHashCode;

        public ContainsRule(IEnumerable<T> items, IEqualityComparer<T> comparer = null)
        {
            Items = new ReadOnlySet<T>(new HashSet<T>(items, comparer));
        }

        public ISet<T> Items { get; private set; }

        public override bool Evaluate(T value) => Items.Contains(value);


        public override bool Equals(IRule<T> other) => other is ContainsRule<T> otherSet && Items.SetEquals(otherSet.Items);

        public override int GetHashCode()
        {
            if (myHashCode == 0)
            {
                int hash = 486187739;

                foreach (var item in Items)
                {
                    hash = (hash * 16777619) ^ item.GetHashCode();
                }

                myHashCode = hash;
            }

            return myHashCode;
        }
    }
}
