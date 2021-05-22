﻿using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Clause : IClause
    {
        private IAttributesModel myAttributesModel;
        private ILinguisticStructureFactory myFactory;

        public Clause(IAdTree clauseAdTree, IAttributesModel attributesModel, ILinguisticStructureFactory factory)
        {
            AdTree = clauseAdTree;
            myAttributesModel = attributesModel;
            myFactory = factory;
        }

        public IAdTree AdTree { get; private set; }

        public BigInteger StructureAttributes => 0;

        public string Value => AdTree.Phrase;


        public ITerm Subject => TryFindSubject();

        public ITerm Predicate => TryFindPredicate();

        private ITerm TryFindSubject()
        {
            ITerm result = null;

            var verb = AdTree.RightChildren.FirstOrDefault(x => myAttributesModel.IsVerb(x.Morpheme.Attributes));
            if (verb != null)
            {
                var valencies = myAttributesModel.GetNumberOfValencies(verb.Morpheme.Attributes);
                if (valencies >= 1)
                {
                    var valency1 = verb.GetSequenceToRoot().FirstOrDefault(x => x.Pattern.ValencyPosition == 1);
                    if (valency1?.Left != null)
                    {
                        result = myFactory.CreateTerm(valency1.Left);
                    }
                }
            }

            return result;
        }

        private ITerm TryFindPredicate()
        {
            ITerm result = null;

            var verb = AdTree.RightChildren.FirstOrDefault(x => myAttributesModel.IsVerb(x.Morpheme.Attributes));
            if (verb != null)
            {
                result = myFactory.CreateTerm(verb);
            }

            return result;
        }
    }
}
