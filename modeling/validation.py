import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
import numpy as np
import pandas as pd
from sklearn.metrics import accuracy_score, classification_report


NAME = "der_eigen_lr_d32_l1e-5"

MODEL_SIZE = 32
INPUT_DIM = 60
LEARNING_RATE = 1e-5
WEIGHT_DECAY = 0
DROPOUT = 0


class Transformer(nn.Module):
    def __init__(self, input_dim=INPUT_DIM, d_model=MODEL_SIZE,
                 nhead=4, num_layers=2, num_classes=2):
        super().__init__()

        # Linear embedding from 80 to d
        self.input_projection = nn.Linear(input_dim, d_model)

        # Encoder
        encoder_layer = nn.TransformerEncoderLayer(d_model=d_model, nhead=nhead, batch_first=True)
        self.transformer_encoder = nn.TransformerEncoder(encoder_layer, num_layers=num_layers)
        self.dropout = nn.Dropout(p=DROPOUT)

        # Classification head
        self.classifier = nn.Linear(d_model, num_classes)

    def forward(self, x):
        batch_size, seq_len, _ = x.shape
        x = self.input_projection(x)
        x = x.permute(1, 0, 2)
        x = self.transformer_encoder(x)
        x = x[-1, :, :]
        logits = self.classifier(x)
        return logits


model = Transformer(input_dim=INPUT_DIM, d_model=MODEL_SIZE,
                    nhead=4, num_layers=2, num_classes=2)
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model.load_state_dict(torch.load("Models/model_"+NAME+".pth", map_location=device))
model.to(device)
model.eval()

valid_npy = np.load("../Data/validation_der2.npy")
valid_labels = valid_npy[:, 0, 0]
valid_labels = (valid_labels == 1).astype(np.int64)
valid_data = valid_npy[:, :, 1:]
valid_tensor = torch.from_numpy(valid_data).float().to(device)

with torch.no_grad():
    logits = model(valid_tensor)
    preds = logits.argmax(dim=1)

acc = accuracy_score(valid_labels, preds)
print("Accuracy:", acc)
print("Classification report:")
print(classification_report(valid_labels, preds))

