using Krino.Vertical.Utils.Collections;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public class Sememe : TreeBase<ISememe>, ISememe
    {
        public Sememe(string category, string value)
        {
            Meaning = category;
            Value = value;
        }

        public string Meaning { get; private set; }

        public string Value { get; private set; }

        public ISememe Hypernymy => Parent;

        public IReadOnlyList<ISememe> Hyponymies => Children;
    }
}
