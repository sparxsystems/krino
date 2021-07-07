using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Clause : LinguisticStructureBase, IClause
    {
        public Clause(IAdTree clauseAdTree, IAttributesModel attributesModel, ILinguisticStructureFactory factory, BigInteger attributes)
            : base(clauseAdTree, attributesModel, factory, attributes)
        {
        }

        public ITerm Subject => TryGetSubject();

        public ITerm Predicate => TryGetPredicate();


        public static bool IsClause(IAdTree adTree, IAttributesModel attributesModel)
        {
            var found = adTree?.GetRightSequence().FirstOrDefault(x => attributesModel.IsVerb(x.Morpheme.Attributes) || attributesModel.IsU(x.Morpheme.Attributes));
            var result = found != null && attributesModel.IsVerb(found.Morpheme.Attributes);
            return result;
        }


        private ITerm TryGetSubject()
        {
            ITerm result = null;

            var startingAdTree = AdTree.Right != null ?
                AdTree :
                AdTree.Left;

            if (startingAdTree != null)
            {
                var verb = startingAdTree.GetRightSequence().FirstOrDefault(x => AttributesModel.IsVerb(x.Morpheme.Attributes));
                if (verb != null)
                {
                    var valencies = AttributesModel.GetNumberOfValencies(verb.Morpheme.Attributes);
                    if (valencies >= 1)
                    {
                        var valency1 = verb.GetSequenceToRoot().FirstOrDefault(x => x.Pattern.ValencyPosition == 1);
                        if (valency1?.Left != null)
                        {
                            var subjectAdTree = valency1.Left.MakeDeepCopy();
                            result = Factory.CreateTerm(subjectAdTree, StructureAttributes.Term.Subject);
                        }
                    }
                }
            }

            return result;
        }

        private ITerm TryGetPredicate()
        {
            ITerm result = null;

            var startingAdTree = AdTree.Right != null ?
                AdTree :
                AdTree.Left;

            if (startingAdTree != null)
            {
                var verb = startingAdTree.GetRightSequence().FirstOrDefault(x => AttributesModel.IsVerb(x.Morpheme.Attributes));
                if (verb != null)
                {
                    IAdTree predicateAdTree;

                    var verbValencies = AttributesModel.GetNumberOfValencies(verb.Morpheme.Attributes);
                    if (verbValencies > 0)
                    {
                        predicateAdTree = verb.GetSequenceToRoot()
                            .First(x => x.AdPosition == null || AttributesModel.IsU(x.AdPosition.Morpheme.Attributes))
                            .MakeDeepCopy();

                        // Remove the subject (i.e. remove the sub-adtree on the first valency)
                        var firstValency = predicateAdTree.GetRightSequence().FirstOrDefault(x => x.Pattern.ValencyPosition == 1);
                        if (firstValency != null)
                        {
                            firstValency.Left = null;
                        }
                    }
                    else
                    {
                        predicateAdTree = verb.MakeDeepCopy();
                        result = Factory.CreateTerm(verb, StructureAttributes.Term.Predicate);
                    }

                    result = Factory.CreateTerm(predicateAdTree, StructureAttributes.Term.Predicate);
                }
            }
            

            return result;
        }
    }
}
