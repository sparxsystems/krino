using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Sentence : LinguisticStructureBase, ISentence
    {
        public Sentence(IAdTree SentenceAdTree, IAttributesModel attributesModel, ILinguisticStructureFactory factory, BigInteger attributes)
            : base(SentenceAdTree, attributesModel, factory, attributes)
        {
        }

        public override BigInteger Attributes => 0;

        public IEnumerable<IClause> Clauses => TryFindClauses();

        private IEnumerable<IClause> TryFindClauses()
        {
            var possibleClauses = AdTree.GetAdTreesInAdTreeOrder().Where(x => AttributesModel.IsU(x.Morpheme.Attributes));

            // If U grammar character is present then there can be multiple clauses.
            if (possibleClauses.Any())
            {
                foreach (var possibleClause in possibleClauses)
                {
                    var isClauseOnRight = Clause.IsClause(possibleClause.Right, AttributesModel);
                    if (isClauseOnRight)
                    {
                        var clauseAdTree = possibleClause.Right.MakeDeepCopy();
                        var clause = Factory.CreateClause(clauseAdTree, 0);
                        yield return clause;
                    }

                    var isClauseOnLeft = Clause.IsClause(possibleClause.Left, AttributesModel);
                    if (isClauseOnLeft)
                    {
                        var clauseAdTree = possibleClause.Left.MakeDeepCopy();
                        var clause = Factory.CreateClause(clauseAdTree, 0);
                        yield return clause;
                    }
                }
            }
            else
            {
                if (Clause.IsClause(AdTree, AttributesModel))
                {
                    var clause = Factory.CreateClause(AdTree, StructureAttributes.Sentence.SimpleSentence);
                    yield return clause;
                }
            }
        }
    }
}
