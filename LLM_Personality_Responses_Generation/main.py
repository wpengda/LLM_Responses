import time
import pandas as pd
from src.model import LLM
from src.utils import majority_vote, post_process

import argparse


def personality_simulation(args):
    # Initialize the LLM for response generation
    lm = LLM(args.api_key, args.model_type, args.model_name, args.voter)

    # Read instructions from CSV files
    instructions = pd.read_csv(args.instruction, delimiter=";")

    # Iterate through each row of the data
    results = []
    column_name = ''
    for index, row in instructions.iterrows():
        time.sleep(1)
        # Generate the prompt
        if 'personal_profile' in instructions.keys():
            prefix = "I'm "
            description = f"{prefix}{row['personal_profile']}."
            column_name = 'personal_profile'
        else:
            prefix = ""
            description = row['persona_description']
            column_name = 'persona_description'

        if args.task == 'force_choice':
            prompt = (f"For the following task, respond in a way that matches this description: \"{description}\" "
                      f"Considering the statement, select one of the following descriptions that is the MOST like you "
                      f"and another one that is the LEAST like you by indicating their index: \"{row['item']}\"")
        elif args.task == 'score_scale':
            prompt = (f"For the following task, respond in a way that matches this description: \"{description}\" "
                      f"Considering the statement, please indicate the extent to which you agree or disagree on a "
                      f"scale from 1 to 5 (where 1 = \"disagree strongly\", 2 = \"disagree a little\", 3 = \"neither "
                      f"agree nor disagree\", 4 = \"agree a little\", and 5 = \"agree strongly\"): \"{row['item']}\"")
        elif args.task == 'real_world_simulation':
            prompt = "For the following task, respond in a way that matches this description: \"" + row[
                'persona_description'] + "I'm " + row['personal_profile'] + (
                         ". Considering the statement, generate a list of 20 different Facebook status updates as this "
                         "person. Each update must be verbose and reflect the person’s character and description. The "
                         "updates should cover, but should not be limited to, the following topics: work, "
                         "family, friends, free time, romantic life, TV / music / media consumption, "
                         "and communication with others.")

        # Obtain the response
        result_voters = lm.get_response(prompt)
        result = majority_vote(result_voters).strip()
        results.append(result)

        if index == 0:
            print('Example prompt:', prompt)
            print('Response', result)

        if int(index) % args.batch_size == 0:
            print("Load {}% prompts".format(int(index) / len(instructions) * 100))
            instructions['result'] = pd.Series(results)
            csv_idx = args.save.index('.csv')
            file_name = args.save[:csv_idx] + '_' + str(index) + args.save[csv_idx:]
            instructions.to_csv(file_name, encoding="utf_8_sig", index=False)

    instructions['result'] = pd.Series(results)
    instructions.to_csv(args.save, encoding="utf_8_sig", index=False)

    post_process(args.save, args.questionnaire, column_name, lm, prompt, prefix)


if __name__ == "__main__":
    argparser = argparse.ArgumentParser()
    argparser.add_argument('--instruction', type=str, default='output/instructions/instruction_example.csv')
    argparser.add_argument('--save', type=str, default='output/results/results_example.csv')
    argparser.add_argument('--questionnaire', type=str, default='bfi60', choices=['hexaco', 'bfi60', 'ipip300', 'force_choice'])
    argparser.add_argument('--task', type=str, default='score_scale', choices=['force_choice', 'score_scale', 'real_world_simulation'])
    argparser.add_argument('--voter', type=int, default=1)
    argparser.add_argument('--model_type', type=str, default='gpt')
    argparser.add_argument('--model_name', type=str, default='gpt-3.5-turbo')
    argparser.add_argument('--api_key', type=str)
    argparser.add_argument('--batch_size', type=int, default=100)
    args = argparser.parse_args()

    personality_simulation(args)
