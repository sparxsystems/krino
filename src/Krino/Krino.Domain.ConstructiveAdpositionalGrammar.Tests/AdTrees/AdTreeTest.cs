using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeTest
    {
        [Test]
        public void Governor()
        {
            AdTree adTree = new AdTree();

            Assert.IsNull(adTree.Governor);

            AdTree governor = new AdTree();
            
            // Attach the governor.
            adTree.Governor = governor;
            Assert.AreEqual(governor, adTree.Governor);
            Assert.AreEqual(adTree, governor.AdPosition);

            // Detach the governor.
            adTree.Governor = null;
            Assert.IsNull(adTree.Governor);
            Assert.IsNull(governor.AdPosition);
        }

        [Test]
        public void Dependent()
        {
            AdTree adTree = new AdTree();

            Assert.IsNull(adTree.Dependent);

            AdTree dependent = new AdTree();

            // Attach dependent.
            adTree.Dependent = dependent;
            Assert.AreEqual(dependent, adTree.Dependent);
            Assert.AreEqual(adTree, dependent.AdPosition);

            // Detach the dependent.
            adTree.Dependent = null;
            Assert.IsNull(adTree.Dependent);
            Assert.IsNull(dependent.AdPosition);
        }

        [Test]
        public void ElementType()
        {
            AdTree adTree = new AdTree()
            {
                Governor = new AdTree()
                {
                    Governor = new AdTree()

                    // Dependent is missing per purpose.
                },
                Dependent = new AdTree()
                {
                    // Governor is missing per purpose.

                    Dependent = new AdTree()
                }
            };

            Assert.AreEqual(AdTreeElementType.AdPosition, adTree.ElementType);
            
            Assert.AreEqual(AdTreeElementType.AdPosition | AdTreeElementType.Governor, adTree.Governor.ElementType);
            Assert.AreEqual(AdTreeElementType.Governor, adTree.Governor.Governor.ElementType);

            Assert.AreEqual(AdTreeElementType.AdPosition | AdTreeElementType.Dependent, adTree.Dependent.ElementType);
            Assert.AreEqual(AdTreeElementType.Dependent, adTree.Dependent.Dependent.ElementType);
        }

        [Test]
        public void RaisedGrammarCharacter()
        {
            AdTree adTree = new AdTree()
            {
                Governor = new AdTree()
                {
                    Governor = new AdTree() { Morpheme = new Morpheme("read") { Character = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Dependent = new AdTree() { Morpheme = new Morpheme("I") { Character = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Dependent = new AdTree()
                {
                    Governor = new AdTree() { Morpheme = new Morpheme("book") { Character = GrammarCharacter.O } },
                    Dependent = new AdTree() { Morpheme = new Morpheme("the") { Character = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            Assert.AreEqual(GrammarCharacter.I, adTree.RaisedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.O, adTree.Dependent.RaisedGrammarCharacter);

            // Note: If it is not an adposition then it shall return own grammar character.
            Assert.AreEqual(GrammarCharacter.I, adTree.Governor.Governor.RaisedGrammarCharacter);
        }


        [Test]
        public void SaturatedValency()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Governor = new AdTree()
                {
                    Governor = new AdTree() { Morpheme = new Morpheme("read") { Character = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Dependent = new AdTree() { Morpheme = new Morpheme("I") { Character = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Dependent = new AdTree()
                {
                    Governor = new AdTree() { Morpheme = new Morpheme("book") { Character = GrammarCharacter.O } },
                    Dependent = new AdTree() { Morpheme = new Morpheme("the") { Character = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            // 1st valency position. (I)
            Assert.AreEqual(1, adTree.Governor.Dependent.SaturatedValency);

            // 2nd valency position. (adposition of 'the book')
            Assert.AreEqual(2, adTree.Dependent.SaturatedValency);

            // Still 2nd valency position. (book)
            Assert.AreEqual(2, adTree.Dependent.Governor.SaturatedValency);

            // This is an adverb 'the' so the saturated valency shall be 0.
            Assert.AreEqual(0, adTree.Dependent.Dependent.SaturatedValency);
        }


        [Test]
        public void SaturatedValencies()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Governor = new AdTree()
                {
                    Governor = new AdTree() { Morpheme = new Morpheme("read") { Character = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Dependent = new AdTree() { Morpheme = new Morpheme("I") { Character = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Dependent = new AdTree()
                {
                    Governor = new AdTree() { Morpheme = new Morpheme("book") { Character = GrammarCharacter.O } },
                    Dependent = new AdTree() { Morpheme = new Morpheme("the") { Character = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            IReadOnlyList<IAdTree> saturation = adTree.Governor.Governor.SaturatedValencies;
            Assert.AreEqual(2, saturation.Count);

            // 1st valency position.
            Assert.IsTrue(adTree.Governor.Dependent == saturation[0]);

            // 2nd valency position.
            Assert.IsTrue(adTree.Dependent == saturation[1]);
        }

        [Test]
        public void Sibling()
        {
            AdTree adTree = new AdTree()
            {
                Governor = new AdTree(),
                Dependent = new AdTree()
                {
                    // Governor is missing per purpose.

                    Dependent = new AdTree()
                }
            };

            Assert.IsNull(adTree.Sibling);

            Assert.AreEqual(adTree.Dependent, adTree.Governor.Sibling);
            Assert.AreEqual(adTree.Governor, adTree.Dependent.Sibling);

            Assert.IsNull(adTree.Dependent.Dependent.Sibling);
        }

        [Test]
        public void Phrase()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree()
            {
                Governor = new AdTree()
                {
                    Governor = new AdTree() { Morpheme = new Morpheme("read") { Character = GrammarCharacter.I, Attributes = Attributes.Bivalent } },
                    Dependent = new AdTree() { Morpheme = new Morpheme("I") { Character = GrammarCharacter.O, Attributes = Attributes.Unergative } }
                },
                Dependent = new AdTree()
                {
                    Governor = new AdTree() { Morpheme = new Morpheme("book") { Character = GrammarCharacter.O } },
                    Dependent = new AdTree() { Morpheme = new Morpheme("the") { Character = GrammarCharacter.A, Attributes = Attributes.AttributiveAdjective } }
                }
            };

            string phrase = string.Join(" ", adTree.Phrase.Select(x => x.Morpheme.Morph));
        }
    }
}
