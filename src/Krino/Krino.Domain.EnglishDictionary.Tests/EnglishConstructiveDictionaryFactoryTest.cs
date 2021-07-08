using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Vertical.Utils.Diagnostic;
using Microsoft.Extensions.Logging;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;

namespace Krino.Domain.EnglishDictionary.Tests
{
    [TestFixture]
    public class EnglishConstructiveDictionaryFactoryTest
    {
        [Test]
        public void Performance()
        {
            //var traceStream = new StreamWriter("/Ondrej/tmp/tracefile.txt");
            //Trace.Logger = new TextWriterLogger(traceStream);

            var factory = new EnglishConstructiveDictionaryFactory()
            {
                MaxWords = 8,
            };

            IConstructiveDictionary dictionary = null;
            try
            {
                //Trace.StartProfiler();

                //var patternConstructions = new PatternConstructions(factory.MaxWords, PatternProvider.Patterns);

                dictionary = factory.Create();
            }
            finally
            {
                Trace.StopProfiler();
                Trace.Logger.LogInformation($"Writings: {DirectedGraphExt.Writings}");
                //Trace.Logger.LogInformation($"Constructions: {dictionary.PatternConstructions}");
            }
       }
    }
}
