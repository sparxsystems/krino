using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalArgumentation
{
    public class Argument
    {
        public Argument(ISentence sentence)
        {
            Sentence = sentence;
        }

        public ISentence Sentence { get; }


        public IClause Premise => Sentence.IndependentClauses[0].DependentClauses.First();

        public IClause Conclusion => Sentence.IndependentClauses[0].IndependentClause;


        public IReadOnlyCollection<ISentence> Lever { get; private set; }
    }
}
