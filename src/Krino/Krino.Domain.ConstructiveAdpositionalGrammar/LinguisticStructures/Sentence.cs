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

        public List<IClause> IndependentClauses { get; } = new List<IClause>();

        public IMorpheme PunctuationMark { get; set; }

        public string Value => string.Join("", string.Join(" ", IndependentClauses.Select(x => x.Value)), PunctuationMark?.Value);

        public string GrammarStr => string.Join("", AttributesStr, "(", string.Join(" ", IndependentClauses.Select(x => x.GrammarStr)), ")");

        public override BigInteger Attributes
        {
            get
            {
                BigInteger result = 0;

                if (IndependentClauses.Count > 0)
                {
                    var dependentClausesExist = IndependentClauses.SelectMany(x => x.DependentClauses).Any();

                    if (IndependentClauses.Count == 1 && !dependentClausesExist)
                    {
                        if (dependentClausesExist)
                        {
                            // One independent clause and at least one dependent clause.
                            result = GrammarAttributes.Sentence.Complex;
                        }
                        else
                        {
                            // Only one independent clause.
                            result = GrammarAttributes.Sentence.Simple;
                        }
                    }
                    else if (dependentClausesExist)
                    {
                        // Two or more independent clauses and at least one dependnet clause.
                        result = GrammarAttributes.Sentence.CompoundComplex;
                    }
                    else
                    {
                        // Two or more independent clauses.
                        result = GrammarAttributes.Sentence.Compound;
                    }
                }

                return result;
            }

            protected set { }
        }
    }
}
