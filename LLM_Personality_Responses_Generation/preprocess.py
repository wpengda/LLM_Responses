import argparse
import random
import csv
from itertools import product
import pandas as pd
import numpy as np


def sort_function(question):
    return int(question.split('.')[0])

def random_persona_selection(persona_save_path, sample_num=250):
    persona_file = open("data/persona_description.txt", "r")

    personas = persona_file.read().lower().splitlines()

    personalities = pd.read_csv("data/personality.csv")
    new_personality_list = []
    for idx, row in personalities.iterrows():
        personality = row['Persona'].lower().strip()
        if personality not in personas:
            new_personality_list.append(row['Persona'] + '\n')

    random_index_list = random.sample(range(0, len(new_personality_list)), sample_num)
    res_list = np.take(new_personality_list, random_index_list)

    with open(persona_save_path, 'w') as file:
        combined_list = ['\n'.join(personas), '\n'] + list(res_list)
        file.writelines(combined_list)


def generate_persona_instructions(persona_save_path, questionnaire):
    persona_file = open(persona_save_path, "r")
    item_file = open(f"data/{questionnaire}_items.txt", "r")

    persona = persona_file.read().splitlines()
    item = item_file.read().splitlines()

    result = list(product(persona, item))

    header = ["persona_description", "item"]
    with open(f'output/instructions/persona_{questionnaire}_instruction.csv', 'w') as file:
        writer = csv.writer(file, delimiter=';', lineterminator='\n')
        writer.writerow(header)
        writer.writerows(result)

    persona_file.close()
    item_file.close()


def generate_personal_profile(profile_save_path, sample_num=None, adj_num=None):
    '''
        sample_num: the number of personal profile in each category
    '''
    qualifier_file = open("data/qualifier.txt", "r")

    adjectival_markers = pd.read_csv("data/adjectival_markers.csv")
    qualifiers = qualifier_file.read().splitlines()

    # Classify adjective words
    adjectives = {}
    for index, row in adjectival_markers.iterrows():
        domain = row['Domain']
        if domain not in adjectives.keys():
            adjectives[domain] = {'low_marker': [row['Low Marker']], 'high_marker': [row['High Marker']]}
        else:
            adjectives[domain]['low_marker'].append(row['Low Marker'])
            adjectives[domain]['high_marker'].append(row['High Marker'])

    with open(profile_save_path, 'a') as file:
        # Generate personal profiles
        for qualifier in qualifiers:
            if qualifier:
                connector = ', ' + qualifier + ' '
                starter = qualifier + ' '
            else:
                connector = ', '
                starter = ''

            for domain, markers in adjectives.items():
                if adj_num:  # Generate personal profiles with a specified number of adj words
                    save_index = []
                    while len(save_index) < sample_num:
                        adj_index_list = random.sample(range(0, len(markers['low_marker'])), adj_num)
                        low_adj_list = np.take(markers['low_marker'], adj_index_list)
                        high_adj_list = np.take(markers['high_marker'], adj_index_list)
                        if adj_index_list not in save_index:
                            save_index.append(adj_index_list)

                            file.write(starter + connector.join(low_adj_list[:-1]) + ' and {} {}\n'.format(qualifier,
                                                                                                           low_adj_list[
                                                                                                               -1]))
                            file.write(starter + connector.join(high_adj_list[:-1]) + ' and {} {}\n'.format(qualifier,
                                                                                                            high_adj_list[
                                                                                                                -1]))
                else:
                    file.write(starter + connector.join(markers['low_marker'][:-1]) + ' and {} {}\n'.format(qualifier,
                                                                                                            markers[
                                                                                                                'low_marker'][
                                                                                                                -1]))
                    file.write(starter + connector.join(markers['high_marker'][:-1]) + ' and {} {}\n'.format(qualifier,
                                                                                                             markers[
                                                                                                                 'high_marker'][
                                                                                                                 -1]))

        # Generate neutral personal profiles
        for domain, markers in adjectives.items():
            if adj_num:
                save_index = []
                while len(save_index) < sample_num / 2:
                    adj_index_list = random.sample(range(0, len(markers['low_marker'])), adj_num)
                    if adj_index_list not in save_index:
                        save_index.append(adj_index_list)
                        low_adj_list = np.take(markers['low_marker'], adj_index_list)
                        high_adj_list = np.take(markers['high_marker'], adj_index_list)

                        adj_list = []
                        for low_marker, high_marker in zip(low_adj_list[:-1], high_adj_list[:-1]):
                            adj_list.append('neither {} nor {}, '.format(low_marker, high_marker))
                        file.write(
                            ''.join(adj_list) + 'and neither {} nor {}\n'.format(low_adj_list[-1], high_adj_list[-1]))
            else:
                adj_list = []
                for low_marker, high_marker in zip(markers['low_marker'][:-1], markers['high_marker'][:-1]):
                    adj_list.append('neither {} nor {}, '.format(low_marker, high_marker))
                file.write(''.join(adj_list) + 'and neither {} nor {}\n'.format(markers['low_marker'][-1],
                                                                                markers['high_marker'][-1]))

    qualifier_file.close()


def generate_shape_instructions(profile_save_path, questionnaire):
    profile_file = open(profile_save_path, "r")
    item_file = open(f"data/{questionnaire}_items.txt", "r")

    profile = profile_file.read().splitlines()
    item = item_file.read().splitlines()

    result = list(product(profile, item))

    header = ["personal_profile", "item"]
    with open(f'output/instructions/shape_{questionnaire}_instruction.csv', 'w') as file:
        writer = csv.writer(file, delimiter=';', lineterminator='\n')
        writer.writerow(header)
        writer.writerows(result)

    profile_file.close()
    item_file.close()


if __name__ == "__main__":
    argparser = argparse.ArgumentParser()
    argparser.add_argument('--description', type=str, choices=['persona_description', 'personal_profile'])
    argparser.add_argument('--sample_num', type=int, required=False)
    argparser.add_argument('--questionnaire', type=str, choices=['bfi60', 'ipip300', 'hexaco', 'force_choice'])
    argparser.add_argument('--persona_save_path', type=str,
                           default='output/description/persona_description.txt')
    argparser.add_argument('--profile_save_path', type=str, default='output/description/personal_profile.txt')

    args = argparser.parse_args()

    # Instruction Generation
    if args.description == 'persona_description':  # Persona simulation
        random_persona_selection(args.persona_save_path)
        generate_persona_instructions(args.persona_save_path, args.questionnaire)
    elif args.description == 'personal_profile':  # Shape simulation
        generate_personal_profile(args.profile_save_path, args.sample_num, 5)
        generate_shape_instructions(args.profile_save_path, args.questionnaire)


