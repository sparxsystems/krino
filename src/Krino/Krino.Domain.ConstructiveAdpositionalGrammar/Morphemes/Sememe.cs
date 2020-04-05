using Krino.Vertical.Utils.Collections;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public class Sememe : TreeComposite<ISememe>, ISememe
    {
        public Sememe(string category, string value)
        {
            Category = category;
            Value = value;
        }

        public string Category { get; private set; }

        public string Value { get; private set; }

        public ISememe Hypernymy => Parent;

        public IReadOnlyList<ISememe> Hyponymies => Children;
    }
}
