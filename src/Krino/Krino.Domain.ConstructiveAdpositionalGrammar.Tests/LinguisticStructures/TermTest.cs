using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.LinguisticStructures
{
    [TestFixture]
    public class TermTest
    {
        private IAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void Words()
        {
            var adTree = new AdTree(new Morpheme(myAttributesModel, "", EnglishAttributes.Epsilon), EnglishPattern.O1_I.SetLeftFirst())
            {
                Right = new AdTree(new Morpheme(myAttributesModel, "read", EnglishAttributes.I.Lexeme), EnglishPattern.I_Lexeme),
                Left = new AdTree(new Morpheme(myAttributesModel, "I", EnglishAttributes.O.Lexeme), EnglishPattern.O_Lexeme),
            };

            var factory = new LinguisticStructureFactory(myAttributesModel);
            var term = factory.CreateTerm(adTree, 0);
            var words = term.Words.ToList();
            Assert.AreEqual(2, words.Count);
            Assert.AreEqual("I", words[0].Value);
            Assert.AreEqual("read", words[1].Value);
        }
    }
}
