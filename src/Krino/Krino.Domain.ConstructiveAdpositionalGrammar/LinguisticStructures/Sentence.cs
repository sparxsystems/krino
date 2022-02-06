using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Sentence : LinguisticStructureBase, ISentence
    {
        public Sentence(BigInteger attributes)
            : base(attributes)
        {
        }

        public List<IClause> Clauses { get; } = new List<IClause>();

        public IMorpheme PunctuationMark { get; set; }

        public string Value => string.Join("", string.Join(" ", Clauses.Select(x => x.Value)), PunctuationMark?.Value);

        public override BigInteger Attributes
        {
            get
            {
                BigInteger result = base.Attributes;

                if (GrammarAttributes.Morpheme.U.Bound.PunctuationMark.QuestionMark.IsIn(base.Attributes))
                {
                    result |= GrammarAttributes.Sentence.Interrogative;
                }
                else if (Clauses.Count == 1 && GrammarAttributes.Clause.Imperative.IsIn(Clauses[0].Attributes) &&
                         GrammarAttributes.Morpheme.U.Bound.PunctuationMark.ExclamationPoint.IsIn(base.Attributes))
                {
                    result |= GrammarAttributes.Sentence.Imperative;
                }

                return result;
            }
            protected set => base.Attributes = value;
        }
    }
}
