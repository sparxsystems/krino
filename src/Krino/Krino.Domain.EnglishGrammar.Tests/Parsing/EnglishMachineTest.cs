using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.Parsing;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.EnglishGrammar.Tests.Parsing
{
    [TestFixture]
    public class EnglishMachineTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void Test()
        {
            var machine = new EnglishMachine();

            var pronoun = new Word(new AdTree(new Morpheme(myAttributesModel, "he", EnglishAttributes.O), EnglishPattern.O_Lexeme_Noun));
            var verb = new Word(new AdTree(new Morpheme(myAttributesModel, "reads", EnglishAttributes.I), EnglishPattern.I_Lexeme_Verb));

            machine.Machine.Fire(pronoun);
            machine.Machine.Fire(verb);
        }
    }
}
