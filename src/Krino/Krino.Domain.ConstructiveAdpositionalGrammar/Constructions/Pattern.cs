using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    public class Pattern : IPattern
    {
        public Pattern(IAdTree rule)
        {
            Rule = rule;
        }

        public IReadOnlyList<IMorpheme> Match
        {
            get
            {
                List<IMorpheme> result = Rule.Phrase.Select(x => x.Morpheme).ToList();
                return result;
            }
        }

        public IAdTree Rule { get; private set; }
    }
}
