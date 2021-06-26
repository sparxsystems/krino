using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Sentence : LinguisticStructureBase, ISentence
    {
        public Sentence(IAdTree SentenceAdTree, IAttributesModel attributesModel, ILinguisticStructureFactory factory)
            : base(SentenceAdTree, attributesModel, factory, 0)
        {
            InitSentenceAttributes();
        }

        public IEnumerable<IClause> Clauses => TryFindClauses();

        private IEnumerable<IClause> TryFindClauses()
        {
            var possibleClauses = AdTree.GetAdTreesInAdTreeOrder().Where(x => AttributesModel.IsU(x.Morpheme.Attributes));

            // If U grammar character is present then there can be multiple clauses.
            if (possibleClauses.Any())
            {
                foreach (var possibleClause in possibleClauses)
                {
                    // On left
                    IClause leftClause = null;
                    var isClauseOnLeft = Clause.IsClause(possibleClause.Left, AttributesModel);
                    if (isClauseOnLeft)
                    {
                        // The left clause will also containt the connecting adposition.
                        var clauseAdTree = possibleClause.MakeDeepCopy();

                        // Remove the clause on the right.
                        clauseAdTree.Right = null;

                        BigInteger clauseAttributes = StructureAttributes.Clause;
                        if (AttributesModel.IsCoordinatingConjunction(clauseAdTree.Morpheme.Attributes))
                        {
                            clauseAttributes |= StructureAttributes.Clause.Independent;
                        }
                        if (AttributesModel.IsSubOrdinatingConjunction(clauseAdTree.Morpheme.Attributes))
                        {
                            clauseAttributes |= StructureAttributes.Clause.Dependent;
                        }
                        if (AttributesModel.IsCauseConjunction(clauseAdTree.Morpheme.Attributes))
                        {
                            clauseAttributes |= StructureAttributes.Clause.Premis;
                        }

                        leftClause = Factory.CreateClause(clauseAdTree, clauseAttributes);
                    }

                    // On right
                    var isClauseOnRight = Clause.IsClause(possibleClause.Right, AttributesModel);
                    if (isClauseOnRight)
                    {
                        var clauseAdTree = possibleClause.Right.MakeDeepCopy();

                        BigInteger clauseAttributes = StructureAttributes.Clause.Independent;
                        if (leftClause != null && StructureAttributes.Clause.Premis.IsIn(leftClause.Attributes))
                        {
                            clauseAttributes |= StructureAttributes.Clause.Conclusion;
                        }

                        // Clause on the right is always independent.
                        var clause = Factory.CreateClause(clauseAdTree, clauseAttributes);
                        yield return clause;
                    }

                    // Note: the left clause is returned as the second to preserve the natural order of clauses.
                    if (leftClause != null)
                    {
                        yield return leftClause;
                    }
                    
                }
            }
            else
            {
                if (Clause.IsClause(AdTree, AttributesModel))
                {
                    var clause = Factory.CreateClause(AdTree, StructureAttributes.Clause.Independent);
                    yield return clause;
                }
            }
        }

        private void InitSentenceAttributes()
        {
            Attributes = StructureAttributes.Sentence;

            var clauses = Clauses.ToList();

            var independentClauses = clauses.Count(x => StructureAttributes.Clause.Independent.IsIn(x.Attributes));
            var dependentClauses = clauses.Count(x => StructureAttributes.Clause.Dependent.IsIn(x.Attributes));

            if (independentClauses == 1 && dependentClauses == 0)
            {
                Attributes |= StructureAttributes.Sentence.SimpleSentence;
            }
            else if (independentClauses == 1 && dependentClauses >= 1)
            {
                Attributes |= StructureAttributes.Sentence.ComplexSentence;
            }
            else if (independentClauses >= 2 && dependentClauses == 0)
            {
                Attributes |= StructureAttributes.Sentence.CompoundSentence;
            }
            else if (independentClauses >=2 && dependentClauses >= 1)
            {
                Attributes |= StructureAttributes.Sentence.CompoundComplexSentence;
            }

        }
    }
}
