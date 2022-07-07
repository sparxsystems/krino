using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    internal class Sentence : LinguisticStructureBase, ISentence
    {
        public Sentence(BigInteger attributes)
            : base(attributes)
        {
        }

        public List<IClause> IndependentClauses { get; } = new List<IClause>();

        public IMorpheme PunctuationMark { get; set; }

        public string Value => string.Join("", string.Join(" ", IndependentClauses.Select(x => x.Value)), PunctuationMark?.Value).Trim();

        public string GrammarStr => string.Join("", AttributesStr, "(", string.Join(" ", IndependentClauses.Select(x => x.GrammarStr)), ")");

        public void BuildFormattedGrammarStr(int indent, StringBuilder builder)
        {
            builder.Append(new string(' ', indent)).Append(Value).Append(" : ").AppendLine(AttributesStr);

            IndependentClauses.ForEach(x => x.BuildFormattedGrammarStr(indent + 4, builder));
            PunctuationMark?.BuildFormattedGrammarStr(indent + 4, builder);
        }

        public override BigInteger Attributes
        {
            get
            {
                BigInteger result = 0;

                if (IndependentClauses.Count > 0)
                {
                    var dependentClauses = IndependentClauses.SelectMany(x => x.DependentClauses);
                    var dependentClausesExist = dependentClauses.Any();

                    if (IndependentClauses.Count == 1)
                    {
                        if (dependentClausesExist)
                        {
                            if (dependentClauses.Any(x => GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Cause.IsIn(x.Conjunction.Attributes)))
                            {
                                result = GrammarAttributes.Sentence.Complex.Argument;
                            }
                            else if (dependentClauses.Any(x => GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating.Sememe.Condition.IsIn(x.Conjunction.Attributes)))
                            {
                                result = GrammarAttributes.Sentence.Complex.Condition;
                            }
                            else
                            {
                                // One independent clause and at least one dependent clause.
                                result = GrammarAttributes.Sentence.Complex;
                            }
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

        public ILinguisticStructure DeepCopy()
        {
            var independentClauses = IndependentClauses.Select(x => x.DeepCopy()).OfType<IClause>();
            var result = new Sentence(Attributes);
            result.IndependentClauses.AddRange(independentClauses);
            return result;
        }
    }
}
