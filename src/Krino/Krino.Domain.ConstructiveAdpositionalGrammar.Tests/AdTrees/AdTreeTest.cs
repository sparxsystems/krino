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
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", Attributes.I), new Pattern()),
                    Left = new AdTree(new Morpheme("I", Attributes.O), new Pattern())
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", Attributes.O), new Pattern()),
                    Left = new AdTree(new Morpheme("the", Attributes.A), new Pattern())
                }
            };


            Assert.AreEqual(GrammarCharacter.I, adTree.InheritedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.e, adTree.Right.Left.InheritedGrammarCharacter);
            Assert.AreEqual(GrammarCharacter.I, adTree.Right.InheritedGrammarCharacter);

            Assert.AreEqual(GrammarCharacter.O, adTree.Left.InheritedGrammarCharacter);
        }

        [Test]
        public void AdPosition_IncorrectParent_Exception()
        {
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Left = new AdTree(new Morpheme("", 0), new Pattern())
            };
            AdTree adTree2 = new AdTree(new Morpheme("", 0), new Pattern());

            // The adTree2 does not references the adTree.RightChild.RightChild therefore it shall throw the exception.
            Assert.Throws<InvalidOperationException>(() => adTree.AdPosition = adTree2);

            // Also it shall not be possible to assign null.
            Assert.Throws<InvalidOperationException>(() => adTree.Left.AdPosition = null);
        }

        [Test]
        public void IsAdPosition()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern()),
                Left = new AdTree(new Morpheme("", 0), new Pattern()),
            };

            Assert.IsTrue(adTree.IsAdPosition);
            Assert.IsFalse(adTree.Right.IsAdPosition);
            Assert.IsFalse(adTree.Left.IsAdPosition);
        }

        [Test]
        public void AdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("hello", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern()),
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
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern());
            Assert.IsNull(adTree.Right);

            AdTree rightChild = new AdTree(new Morpheme("", 0), new Pattern());
            
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
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern());
            Assert.IsNull(adTree.Left);

            AdTree leftChild = new AdTree(new Morpheme("", 0), new Pattern());

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
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("the", 0), new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("the", 0), new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("I", 0), new Pattern())
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", 0), new Pattern()),
                    Left = new AdTree(new Morpheme("the", 0), new Pattern())
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
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", Attributes.I), Pattern.Morpheme(Attributes.I)),
                    Left = new AdTree(new Morpheme("I", Attributes.O), Pattern.Morpheme(Attributes.O))
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", Attributes.O), Pattern.Morpheme(Attributes.O)),
                    Left = new AdTree(new Morpheme("the", Attributes.A), Pattern.Morpheme(Attributes.A))
                }
            };

            // the governor of 'I' is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Right.Left.GetGovernor());

            // the governor of 'book'-adposition is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Left.GetGovernor());

            // the governor of 'book' is the 'read'.
            // Note: the book is already the governor so the governor of the governor shall be found.
            Assert.IsTrue(adTree.Right.Right == adTree.Left.Right.GetGovernor());

            // the governor of 'the' is the 'book'.
            Assert.IsTrue(adTree.Left.Right == adTree.Left.Left.GetGovernor());

            // the governor of the root is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.GetGovernor());

            // the governor of the 'read'-adposition is the 'read'.
            Assert.IsTrue(adTree.Right.Right == adTree.Right.GetGovernor());
        }

        [Test]
        public void IsGovernor()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", Attributes.I), Pattern.Morpheme(Attributes.I)),
                    Left = new AdTree(new Morpheme("I", Attributes.O), Pattern.Morpheme(Attributes.O))
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", Attributes.O), Pattern.Morpheme(Attributes.O)),
                    Left = new AdTree(new Morpheme("the", Attributes.A), Pattern.Morpheme(Attributes.A))
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
        public void DependentAdPositions()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme("", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("read", Attributes.I), Pattern.Morpheme(Attributes.I)),
                    Left = new AdTree(new Morpheme("I", Attributes.O), Pattern.Morpheme(Attributes.O))
                },
                Left = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("book", Attributes.O), Pattern.Morpheme(Attributes.O)),
                    Left = new AdTree(new Morpheme("the", Attributes.A), Pattern.Morpheme(Attributes.A))
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
        public void Phrase()
        {
            // The phrase: I read the book.
            AdTree adTree = new AdTree(new Morpheme(".", 0), new Pattern())
            {
                Right = new AdTree(new Morpheme("", 0), new Pattern())
                {
                    Right = new AdTree(new Morpheme("", 0),
                        new Pattern() { LeftRule = MorphemeRule.O_Lexeme.SetOrder(1) })
                    {
                        Right = new AdTree(new Morpheme("read", 0), new Pattern()),
                        Left = new AdTree(new Morpheme("I", 0), new Pattern())
                    },
                    Left = new AdTree(new Morpheme("", 0),
                        new Pattern() { LeftRule = MorphemeRule.A_Lexeme.SetOrder(1) })
                    {
                        Right = new AdTree(new Morpheme("book", 0), new Pattern()),
                        Left = new AdTree(new Morpheme("the", 0), new Pattern())
                    }
                }
            };

            string phrase = adTree.Phrase;
            Assert.AreEqual("I read the book .", phrase);
        }
    }
}
