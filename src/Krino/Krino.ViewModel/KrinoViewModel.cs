using Krino.Domain.ConstructiveArgumentation;
using Krino.Domain.ConstructiveGrammar.Parsing;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.ViewModel
{
    public class KrinoViewModel
    {
        private Parser myParser;
        private Argumentation myArgumentation;

        public KrinoViewModel(Parser parser)
        {
            myParser = parser;
            myArgumentation = new Argumentation();
        }

        public List<LogEntry> Log { get; private set; } = new List<LogEntry>();

        public void ProcessInput(string value)
        {
            if (!string.IsNullOrEmpty(value))
            {
                var userLogEntry = new LogEntry()
                {
                    Name = "USER",
                    Value = value,
                };
                Log.Add(userLogEntry);

                ResponseFor(value);
            }
        }

        private void ResponseFor(string value)
        {
            var response = new LogEntry()
            {
                Name = "KRINO",
            };

            var logValue = new StringBuilder();

            var text = myParser.Parse(value).FirstOrDefault();

            if (text != null && text.Sentences.Count > 0)
            {
                if (text.Sentences.Count == 1)
                {
                    var sentence = text.Sentences[0];
                    var argument = myArgumentation.TryGetArgument(sentence);
                    if (argument != null)
                    {
                        logValue.AppendLine($"Yes, the sentence is an argument.");
                        logValue.AppendLine($"The premise is: '{argument.Premise.Value}'");
                        logValue.AppendLine($"The conclusion is: '{argument.Conclusion.Value}'");
                        logValue.AppendLine($"The argument form is: {argument.Form.ToString().Replace("_", " ")}.");
                    }
                    else
                    {
                        logValue.AppendLine($"The sentence is not an argument.");
                    }
                }
                else
                {
                    logValue.AppendLine("I can analyse only one argument.");
                }

                logValue.AppendLine().AppendLine("Here are details:");
                text.BuildFormattedGrammarStr(0, logValue);
            }
            else
            {
                logValue.AppendLine("I am not able to read the sentence.");
            }

            response.Value = logValue.ToString();
            Log.Add(response);
        }
    }
}
