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
        [Test]
        public void Test()
        {
            var machine = new EnglishMachine();

            machine.Machine.Fire(EnglishAttributes.O.Lexeme.Noun);
            machine.Machine.Fire(EnglishAttributes.I.Lexeme.Verb);
        }
    }
}
