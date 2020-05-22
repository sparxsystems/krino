using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.AdTrees
{
    [TestFixture]
    public class AdTreeTest
    {
        [Test]
        public void InheritedGrammarCharacter()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A }, new Pattern())
                }
            };


            Assert.AreEqual(GrammarCharacter.I, adTree.InheritedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.Epsilon, adTree.Right.Left.InheritedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.I, adTree.Right.InheritedGrammarCharacter);

            Assert.AreEqual(GrammarCharacter.O, adTree.Left.InheritedGrammarCharacter);
        }

        [Test]
        public void AdPosition_IncorrectParent_Exception()
        {
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Left = new AdTree(new Morpheme(""), new Pattern())
            };
            AdTree adTree2 = new AdTree(new Morpheme(""), new Pattern());

            // The adTree2 does not references the adTree.RightChild.RightChild therefore it shall throw the exception.
            Assert.Throws<InvalidOperationException>(() => adTree.AdPosition = adTree2);

            // Also it shall not be possible to assign null.
            Assert.Throws<InvalidOperationException>(() => adTree.Left.AdPosition = null);
        }

        [Test]
        public void IsAdPosition()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern()),
                Left = new AdTree(new Morpheme(""), new Pattern()),
            };

            Assert.IsTrue(adTree.IsAdPosition);
            Assert.IsFalse(adTree.Right.IsAdPosition);
            Assert.IsFalse(adTree.Left.IsAdPosition);
        }

        [Test]
        public void AdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("hello"), new Pattern()),
                    Left = new AdTree(new Morpheme(""), new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern()),
            };

            List<IAdTree> adPositions = adTree.AdPositions.ToList();
            Assert.AreEqual(0, adPositions.Count);

            adPositions = adTree.Right.Right.AdPositions.ToList();
            Assert.AreEqual(2, adPositions.Count);
            Assert.AreEqual(adTree.Right, adPositions[0]);
            Assert.AreEqual(adTree, adPositions[1]);
        }


        [Test]
        public void RightChild()
        {
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern());
            Assert.IsNull(adTree.Right);

            AdTree rightChild = new AdTree(new Morpheme(""), new Pattern());
            
            // Attach the governor.
            adTree.Right = rightChild;
            Assert.AreEqual(rightChild, adTree.Right);
            Assert.AreEqual(adTree, rightChild.AdPosition);

            // Detach the governor.
            adTree.Right = null;
            Assert.IsNull(adTree.Right);
            Assert.IsNull(rightChild.AdPosition);
        }

        [Test]
        public void LeftChild()
        {
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern());
            Assert.IsNull(adTree.Left);

            AdTree leftChild = new AdTree(new Morpheme(""), new Pattern());

            // Attach dependent.
            adTree.Left = leftChild;
            Assert.AreEqual(leftChild, adTree.Left);
            Assert.AreEqual(adTree, leftChild.AdPosition);

            // Detach the dependent.
            adTree.Left = null;
            Assert.IsNull(adTree.Left);
            Assert.IsNull(leftChild.AdPosition);
        }


        [Test]
        public void RightChildren()
        {
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read"), new Pattern()),
                    Left = new AdTree(new Morpheme("I"), new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book"), new Pattern()),
                    Left = new AdTree(new Morpheme("the"), new Pattern())
                }
            };

            List<IAdTree> rightChildren = adTree.RightChildren.ToList();
            Assert.AreEqual(2, rightChildren.Count);
            Assert.AreEqual(adTree.Right, rightChildren[0]);
            Assert.AreEqual(adTree.Right.Right, rightChildren[1]);


            rightChildren = adTree.Right.Right.RightChildren.ToList();
            Assert.AreEqual(0, rightChildren.Count);
        }

        [Test]
        public void IsOnRight()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read"), new Pattern()),
                    Left = new AdTree(new Morpheme("I"), new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book"), new Pattern()),
                    Left = new AdTree(new Morpheme("the"), new Pattern())
                }
            };

            Assert.IsTrue(adTree.Right.IsOnRight);
            Assert.IsFalse(adTree.Left.IsOnRight);
            Assert.IsFalse(adTree.IsOnRight);
        }

        [Test]
        public void IsOnLeft()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read"), new Pattern()),
                    Left = new AdTree(new Morpheme("I"), new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book"), new Pattern()),
                    Left = new AdTree(new Morpheme("the"), new Pattern())
                }
            };

            Assert.IsFalse(adTree.Right.IsOnLeft);
            Assert.IsTrue(adTree.Left.IsOnLeft);
            Assert.IsFalse(adTree.IsOnLeft);
        }

        [Test]
        public void Governor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A }, new Pattern())
                }
            };

            // the governor of 'I' is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Right.Left.Governor);

            // the governor of 'book'-adposition is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Left.Governor);

            // the governor of 'book' is the 'read'.
            // Note: the book is already the governor so the governor of the governor shall be found.
            Assert.IsTrue(adTree.Right.Right == adTree.Left.Right.Governor);

            // the governor of 'the' is the 'book'.
            Assert.IsTrue(adTree.Left.Right == adTree.Left.Left.Governor);

            // the governor of the root is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Governor);

            // the governor of the 'read'-adposition is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Right.Governor);
        }

        [Test]
        public void Governor2()
        {
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    // note: it is empty to ensure the root does not have any governor.
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O }, new Pattern()),
                    Left = new AdTree(new Morpheme(""), new Pattern())
                    {
                        Right = new AdTree(new Morpheme("and") { Attributes = Attributes.U }, new Pattern()),
                        Left = new AdTree(new Morpheme("bla") { Attributes = Attributes.E }, new Pattern())
                    }
                }
            };

            // the governor of 'bla' is the 'book'. (adposition U - 'bla' shall be ignored.
            Assert.IsTrue(adTree.Left.Right == adTree.Left.Left.Left.Governor);

            // the 'book' does not have any governor.
            Assert.IsTrue(adTree.Left.Right.Governor == null);

            // the root does not have any governor.
            Assert.IsTrue(adTree.Governor == null);
        }

        [Test]
        public void IsGovernor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A }, new Pattern())
                }
            };

            // 'read' is governor.
            Assert.IsTrue(adTree.Right.Right.IsGovernor);

            // 'book'-adposition is not the governor.
            Assert.IsFalse(adTree.Left.IsGovernor);

            // root is not the governor.
            Assert.IsFalse(adTree.IsGovernor);
        }

        [Test]
        public void ISGovernor2()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("hello"), new Pattern()),
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O }, new Pattern()),
                    Left = new AdTree(new Morpheme(""), new Pattern())
                    {
                        Right = new AdTree(new Morpheme("and") { Attributes = Attributes.U }, new Pattern()),
                        Left = new AdTree(new Morpheme("bla") { Attributes = Attributes.E }, new Pattern())
                    }
                }
            };

            // hello is not the governor.
            Assert.IsFalse(adTree.Right.Right.IsGovernor);

            // adposition 'and' is not the governor.
            Assert.IsFalse(adTree.Left.Left.Right.IsGovernor);
        }

        [Test]
        public void DependentAdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(""), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read") { Attributes  = Attributes.I }, new Pattern()),
                    Left = new AdTree(new Morpheme("I") { Attributes = Attributes.O }, new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book") { Attributes = Attributes.O }, new Pattern()),
                    Left = new AdTree(new Morpheme("the") { Attributes = Attributes.A }, new Pattern())
                }
            };

            // dependents for 'read'
            List<IAdTree> dependentAdPositions = adTree.Right.Right.DependentAdPositions.ToList();
            Assert.AreEqual(2, dependentAdPositions.Count);
            Assert.IsTrue(adTree.Right == dependentAdPositions[0]);
            Assert.IsTrue(adTree == dependentAdPositions[1]);

            // dependents for 'book'
            dependentAdPositions = adTree.Left.Right.DependentAdPositions.ToList();
            Assert.AreEqual(1, dependentAdPositions.Count);
            Assert.IsTrue(adTree.Left == dependentAdPositions[0]);
        }

        [Test]
        public void ValencyAdPosition()
        {
            AdTree adTree = new AdTree(new Morpheme(""),
                new Pattern() { MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(2) })
            {
                Right = new AdTree(new Morpheme(""),
                    new Pattern() { MorphemeRule = MorphemeRule.Epsilon.SetValencyPosition(1) })
                {
                    Right = new AdTree(new Morpheme("read"), new Pattern()),
                    Left = new AdTree(new Morpheme("I"), new Pattern())
                },
                Left = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book"), new Pattern()),
                    Left = new AdTree(new Morpheme("the"), new Pattern())
                }
            };

            //'I'
            Assert.IsTrue(adTree.Right == adTree.Right.Left.ValencyAdPosition);

            // book
            Assert.IsTrue(adTree == adTree.Left.Right.ValencyAdPosition);

            // the
            Assert.IsTrue(adTree == adTree.Left.Left.ValencyAdPosition);

            // book-adposition shall return root
            Assert.IsTrue(adTree == adTree.Left.ValencyAdPosition);

            // root has defined the valency position so it should return itself
            Assert.IsTrue(adTree == adTree.ValencyAdPosition);

            // read shall return null - because it is the verb
            Assert.IsTrue(null == adTree.Right.Right.ValencyAdPosition);

            Assert.That(true, Is.True);
        }



        [Test]
        public void Phrase()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme("."), new Pattern())
            {
                Right = new AdTree(new Morpheme(""), new Pattern())
                {
                    Right = new AdTree(new Morpheme(""),
                        new Pattern() { LeftRule = MorphemeRule.O_Lexeme.SetOrder(1) })
                    {
                        Right = new AdTree(new Morpheme("read"), new Pattern()),
                        Left = new AdTree(new Morpheme("I"), new Pattern())
                    },
                    Left = new AdTree(new Morpheme(""),
                        new Pattern() { LeftRule = MorphemeRule.A_Lexeme.SetOrder(1) })
                    {
                        Right = new AdTree(new Morpheme("book"), new Pattern()),
                        Left = new AdTree(new Morpheme("the"), new Pattern())
                    }
                }
            };

            string phrase = adTree.Phrase;
            Assert.AreEqual("I read the book .", phrase);
        }
    }
}
